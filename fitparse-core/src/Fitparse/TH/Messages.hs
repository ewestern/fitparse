{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}


module Fitparse.TH.Messages where

import qualified Data.Text as T
import Data.Text.Read
import Data.Text (Text)
import Data.Either
import Data.Char (toLower, isSpace)
import Data.List
import Data.Csv hiding (Name)
import Data.Text.Read
import Data.Maybe
import qualified Data.ByteString.Lazy.Char8 as BSL
import qualified Data.Vector as V
import Data.Serialize.Get
import Control.Monad (join)
import Control.Applicative
import Control.Lens (declareLenses)

import Language.Haskell.TH
import Language.Haskell.TH.Quote
import Language.Haskell.TH.Syntax

import Fitparse.TH.Types (typeifyName, parseBaseType, getInternalTypeName)
import Fitparse.FitType
import Fitparse.BaseTypes

type RawMessageRow = [Text]
type MessageRow = (Text, Text, Text, Text, Text)

dropColumns :: RawMessageRow -> MessageRow 
dropColumns (a:b:c:d:e:_) = (a, b, c, d, e)


recordName :: String -> String
recordName (x:xs) = (toLower x):xs
recordName [] = error "Shouldn't happen."

isArray :: String -> Bool
isArray [] = False
isArray (x:xs) = x == '[' && (last xs) == ']'

makeRecord :: String -> MessageRow -> Q (Maybe (Int, VarBangType))
makeRecord typName (_, fieldNum, recName, typ, arr)  = do
    Just innerTypeName <- lookupType typ
    Just maybe <- lookupType "Maybe"
    Just vec <- lookupType "Vector"
    let bang = Bang NoSourceUnpackedness NoSourceStrictness
    recordName <- newName $  (recordName $ typName <> (typeifyName recName))
    let container = if isArray $ T.unpack arr then vec else maybe
    let vbt = (recordName, bang, AppT (ConT container) (ConT innerTypeName))
    case decimal fieldNum of 
      Right (v, _) ->  return $ Just (v, vbt)
      Left _ -> return Nothing

lookupType :: Text -> Q (Maybe Name)
lookupType text = do
  -- first, check if this is one of our generated types
  name <- lookupTypeName $ typeifyName text
  case name of
    Just v ->  return name
    Nothing -> 
      case parseBaseType text of
        Just bt -> getInternalTypeName bt
        Nothing -> return Nothing
    

parseTypeOpt :: FitType a => Get (Maybe a)
parseTypeOpt = lookaheadOpt typeParser

lookaheadOpt :: Get a -> Get (Maybe a)
lookaheadOpt get = do
  empty <- isEmpty
  if empty
     then return Nothing
     else lookAheadM $ fmap Just get


try :: Alternative m => Get a -> Get (m a)
try get = (fmap pure get) <|> (return empty)

makeInstances :: Name -> Name -> [(Int, Name)] -> Q [Dec]
makeInstances messageName constructorName lensNames = do
  semi <- mkSemiInstance
  mon <- mkMonoidInstance
  fit <- mkFitMsgInstance
  return $ [semi, mon, fit]

  where 
      mkFitMsgInstance :: Q Dec
      mkFitMsgInstance = do
        Just cn <- lookupTypeName "FitMessage"
        Just parserName <- lookupValueName "messageParserByFieldNumber"
        Just mempty <- lookupValueName "mempty"
        Just ret <- lookupValueName "return" 
        varName <- newName "i"
        matches <- traverse mkMatch lensNames
        let wildMatch = Match WildP (NormalB $ AppE (VarE ret) $ VarE mempty) []
        let caseexp = CaseE (VarE varName) $ matches ++ [wildMatch]
        let fund = FunD parserName [Clause [VarP varName] (NormalB caseexp) []]
        return $ InstanceD Nothing [] (AppT (ConT cn) (ConT messageName)) [fund]


      mkMatch :: (Int, Name) -> Q Match
      mkMatch (fieldNum, name) = do
          Just mEmpty <- lookupValueName "mempty" 
          Just try' <- lookupValueName "try" 
          Just tp <- lookupValueName "typeParser" 
          Just ret <- lookupValueName "return" 
          Just dolla <- lookupValueName "$" 
          Just set <- lookupValueName "Control.Lens.set" 
          var  <- newName "v"
          -- ((set lensName) v) mempty
          let recUpdate = AppE (AppE (AppE (VarE set) (VarE name)) (VarE var)) (VarE mEmpty)
          -- return $ ((set lensName) v) mempty
          let retExp = UInfixE (VarE ret) (VarE dolla) recUpdate
          -- do v <- try typeParser ; return $ ((set lensName) v) mempty
          let doExp = DoE [BindS (VarP var) (AppE (VarE try') (VarE tp)), NoBindS retExp]
          return $ Match (LitP $ IntegerL $ fromIntegral fieldNum) (NormalB doExp) []

      mkMonoidInstance :: Q Dec
      mkMonoidInstance = do
        Just cn <- lookupTypeName "Monoid"
        Just mEmpty <- lookupValueName "mempty"
        Just empty <- lookupValueName "empty"
        let empties = replicate (length lensNames) empty
        let exp = foldl (\exp name -> AppE exp (VarE name)) (ConE constructorName) $ empties
        let fund = FunD mEmpty [Clause [] (NormalB exp) []]
        return $ InstanceD Nothing [] (AppT (ConT cn) (ConT messageName)) [fund]

      range = do
          l1 <- ['a'..'z']
          l2 <- ['a'..'z']
          return $ l1:[l2]
      mkSemiInstance :: Q Dec
      mkSemiInstance = do
        Just cn <- lookupTypeName "Semigroup"
        Just mConcat <- lookupValueName "<>"
        Just alternative <- lookupValueName "<|>"
        leftNames <- traverse newName $ fmap (\s -> s ++ "1") $ take (length lensNames)  range
        rightNames <- traverse newName $ fmap (\s -> s ++ "2") $ take (length lensNames) range 
        let leftPat = ConP constructorName $ fmap VarP leftNames
        let rightPat = ConP constructorName $ fmap VarP rightNames
        let exps = fmap (\(l, r) -> UInfixE (VarE l) (VarE alternative) (VarE r) ) $ leftNames `zip` rightNames
        let exp = foldl (\exp n -> AppE exp  n) (ConE constructorName) $ exps
        let fund = FunD mConcat [Clause [leftPat, rightPat] (NormalB exp) []]
        return $ InstanceD Nothing [] (AppT (ConT cn) (ConT messageName)) [fund]

  
--orElse :: Maybe a -> Maybe a -> Maybe a
--orElse ma mb = case ma of
--                 Just x -> ma
--                 Nothing -> mb

makeMessageType :: [MessageRow] -> Q ([Dec], Exp, Con)
makeMessageType [] = fail "Should not get empty MessageRow list."
makeMessageType ((messageName, _, _, _, _):xs) = do
  let name = typeifyName messageName
  Just messageNumName <- lookupValueName $ "MesgNum" <>  name
  tName <- newName $ name <> "Message"
  mName <- newName $ name <> "Message"
  unsortedRecords <- traverse (makeRecord name) xs
  let records =  sortOn fst $ catMaybes unsortedRecords
  Just showName <- lookupTypeName "Show"
  Just eqName <- lookupTypeName "Eq"
  let datadec = DataD [] tName [] Nothing [RecC mName $ fmap snd records] [DerivClause Nothing [ConT showName, ConT eqName]]
  lens <- declareLenses $ return [datadec]
  let lensNames = fmap (\((i,r),n) -> (i, n)) $ zip records $ getLensNames lens
  inst <- makeInstances tName mName lensNames
  -- TopLevel constructor name
  let bang = Bang NoSourceUnpackedness NoSourceStrictness
  cName <- newName $ "Data" <> name <> "Message"
  let cons = NormalC cName [(bang, ConT tName)]
  -- partial for parser map
  Just fmapName <- lookupValueName "fmap"
  Just compose <- lookupValueName "."
  Just mpName <- lookupValueName "messageParserByFieldNumbers"
  let tup = TupE [ConE messageNumName, UInfixE (AppE (VarE fmapName) (ConE cName)) (VarE compose) (VarE mpName)]
  return $ (lens ++ inst, tup, cons)

getLensNames :: [Dec] -> [Name]
getLensNames [] = []
getLensNames ((FunD n cs):xs) = n:(getLensNames xs)
getLensNames (_:xs) = getLensNames xs

partitionRows :: [MessageRow] -> [[MessageRow]]
partitionRows [] = []
partitionRows rows =
  let (x:xs) = dropWhile (\row -> isBlank row || isSectionRow row) rows
      (records, remainder) = span isValRow xs
  in (x:records):(partitionRows remainder)
  where
     isBlank (a, b, c, d, e) = all ((==) T.empty) [a, b, c, d, e]
     isTypeRow (typName, _, _, _, _) = typName /= T.empty
     isValRow (_, idx, recName, baseType, _) = recName /= T.empty && baseType /= T.empty
     isSectionRow (_, _, c, section, _) = section /= T.empty && c == T.empty

fitMessageQ :: String -> Q [Dec]
fitMessageQ str = 
 let rows :: Either String (V.Vector RawMessageRow) = decode NoHeader $ BSL.pack str
 in case rows of
    Right v -> do
      decs <- traverse makeMessageType $ partitionRows $ V.toList $ fmap dropColumns v
      let (dataDecs, tups, constructors) = unzip3 decs
      -- outer type
      dataName <- newName "DataMessageContents"
      Just showName <- lookupTypeName "Show"
      Just eqName <- lookupTypeName "Eq"
      let msgDec = DataD [] dataName [] Nothing constructors [DerivClause Nothing [ConT showName, ConT eqName]]
      
      -- parser map
      Just mapFromList <- lookupValueName "Data.Map.fromList"
      let mapExp = AppE (VarE mapFromList) (ListE tups)
      mapName <- newName "parserMap"
      let valDec = ValD (VarP mapName) (NormalB mapExp) []
      
      return $ (join dataDecs) ++ [msgDec, valDec]

    Left e -> fail e

fitmsg :: QuasiQuoter
fitmsg = quoteFile $ QuasiQuoter 
  { quoteExp = notHandled
  , quotePat = notHandled
  , quoteType = notHandled
  , quoteDec = fitMessageQ
  }
  where notHandled things = error $
          things ++ " are not handled by the regex quasiquoter."
  
