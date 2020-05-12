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
type MessageRow = (Text, Text, Text, Text, Text, Text, Text)

dropColumns :: RawMessageRow -> MessageRow 
dropColumns (a:b:c:d:e:_:g:h:_) = (a, b, c, d, e, g, h)


recordName :: String -> String
recordName (x:xs) = (toLower x):xs
recordName [] = error "Shouldn't happen."

isArray :: String -> Bool
isArray [] = False
isArray (x:xs) = x == '[' && (last xs) == ']'

extractScale :: Text -> Maybe Float
extractScale t = case rational t of
                   Right (v, _) -> Just v
                   Left _ -> Nothing

extractOffset :: Text -> Maybe Float
extractOffset t = case rational t of
                    Right (v, _) -> Just v
                    Left _ -> Nothing

guardFloat :: Text -> (Text -> Maybe Float) -> Text ->  Maybe Float
guardFloat typ func = 
  if elem typ ["sint8", "sint16", "sint32", "sint64"
              , "uint8", "uint16", "uint32", "uint64"
              , "float32", "float64"]
     then func
     else const Nothing



makeRecord :: String -> MessageRow -> Q (Maybe (Int, VarBangType, Type, Maybe Float, Maybe Float))
makeRecord typName (_, fieldNum, recName, typ, arr, scale, offset)  = do
    Just maybe <- lookupType "Maybe"
    Just vec <- lookupType "Vector"
    let bang = Bang NoSourceUnpackedness NoSourceStrictness
    recordName <- newName $  (recordName $ typName <> (typeifyName recName))
    let container = if isArray $ T.unpack arr then vec else maybe
    let maybeScale = guardFloat typ extractScale scale
    let maybeOffset = guardFloat typ extractOffset offset
    Just rawType <- lookupType typ
    Just finalType <- if length (catMaybes [maybeScale, maybeOffset]) > 0
                        then do
                          Just fn <- lookupTypeName "Float"
                          pure $ Just (ConT fn)
                        else return $ Just $ ConT rawType
    let vbt = (recordName, bang, AppT (ConT container) finalType)
    case decimal fieldNum of 
      Right (v, _) ->  return $ Just (v, vbt, AppT (ConT container) (ConT rawType), maybeScale, maybeOffset)
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

makeInstances :: Name -> Name -> [(Int, Name, Type, Maybe Float, Maybe Float)] -> Q [Dec]
makeInstances messageName constructorName recordInfo = do
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
        matches <- traverse mkMatch recordInfo
        let wildMatch = Match WildP (NormalB $ AppE (VarE ret) $ VarE mempty) []
        let caseexp = CaseE (VarE varName) $ matches ++ [wildMatch]
        let fund = FunD parserName [Clause [VarP varName] (NormalB caseexp) []]
        return $ InstanceD Nothing [] (AppT (ConT cn) (ConT messageName)) [fund]


      mkMatch :: (Int, Name, Type, Maybe Float, Maybe Float) -> Q Match
      mkMatch (fieldNum, lensName, rawType, maybeScale, maybeOffset) = do
          Just mEmpty <- lookupValueName "mempty" 
          Just try' <- lookupValueName "try" 
          Just tp <- lookupValueName "typeParser" 
          Just rtf <- lookupValueName "realToFrac" 
          Just ret <- lookupValueName "return" 
          Just dolla <- lookupValueName "$" 
          Just set <- lookupValueName "Control.Lens.set" 
          Just compose <- lookupValueName "."
          var  <- newName "v"
          Just flip' <- lookupValueName "flip"
          Just fmap' <- lookupValueName "fmap"
          scaled <- case maybeScale of
                      Just scale -> do
                          Just div <- lookupValueName "/"
                          let divE = (AppE (AppE (VarE flip') (VarE div)) (LitE $ RationalL $ realToFrac scale))
                          pure $  AppE (VarE fmap') $ UInfixE divE (VarE compose) (VarE rtf)
                          -- (fmap ((*) 123.0) . realToFrac)
                      Nothing -> [|fmap id |]
          offsetted <- case maybeOffset of
                         Just off -> do
                              Just minus <- lookupValueName "-"
                              let minusE = (AppE (AppE (VarE flip') (VarE minus)) (LitE $ RationalL $ realToFrac off))
                              pure $ AppE (VarE fmap') $ UInfixE minusE (VarE compose) (VarE rtf)
                              -- (fmap ((+) 1234.0) . realToFrac)
                         Nothing -> [|fmap id |]

          let composed = UInfixE scaled (VarE compose) offsetted
          let applied = AppE composed (VarE var)

          -- ((set lensName) v) mempty
          let recUpdate = AppE (AppE (AppE (VarE set) (VarE lensName)) applied) (VarE mEmpty)
          -- return $ ((set lensName) v) mempty
          let retExp = UInfixE (VarE ret) (VarE dolla) recUpdate
          -- do v <- try typeParser ; return $ ((set lensName) v) mempty
          let typedVar = SigP (VarP var) rawType
          let doExp = DoE [BindS typedVar (AppE (VarE try') (VarE tp)), NoBindS retExp]
          return $ Match (LitP $ IntegerL $ fromIntegral fieldNum) (NormalB doExp) []

      mkMonoidInstance :: Q Dec
      mkMonoidInstance = do
        Just cn <- lookupTypeName "Monoid"
        Just mEmpty <- lookupValueName "mempty"
        Just empty <- lookupValueName "empty"
        let empties = replicate (length recordInfo) empty
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
        leftNames <- traverse newName $ fmap (\s -> s ++ "1") $ take (length recordInfo)  range
        rightNames <- traverse newName $ fmap (\s -> s ++ "2") $ take (length recordInfo) range 
        let leftPat = ConP constructorName $ fmap VarP leftNames
        let rightPat = ConP constructorName $ fmap VarP rightNames
        let exps = fmap (\(l, r) -> UInfixE (VarE l) (VarE alternative) (VarE r) ) $ leftNames `zip` rightNames
        let exp = foldl (\exp n -> AppE exp  n) (ConE constructorName) $ exps
        let fund = FunD mConcat [Clause [leftPat, rightPat] (NormalB exp) []]
        return $ InstanceD Nothing [] (AppT (ConT cn) (ConT messageName)) [fund]

  
makeMessageType :: [MessageRow] -> Q ([Dec], Exp, Con)
makeMessageType [] = fail "Should not get empty MessageRow list."
makeMessageType ((messageName, _, _, _, _, _, _):xs) = do
  let name = typeifyName messageName
  Just messageNumName <- lookupValueName $ "MesgNum" <>  name
  tName <- newName $ name <> "Message"
  mName <- newName $ name <> "Message"
  unsortedRecords <- traverse (makeRecord name) xs
  let records =  sortOn (\(i, _, _, _, _) -> i) $ catMaybes unsortedRecords
  Just showName <- lookupTypeName "Show"
  Just eqName <- lookupTypeName "Eq"
  let varBangTypes = fmap (\(_, v, _, _, _) -> v) records
  let datadec = DataD [] tName [] Nothing [RecC mName $ varBangTypes] [DerivClause Nothing [ConT showName, ConT eqName]]
  lens <- declareLenses $ return [datadec]
  let recordInfo = fmap (\(i, (n, _, _), rt,  s, o) -> (i, n, rt, s, o)) records 
  inst <- makeInstances tName mName recordInfo
  -- TopLevel constructor name
  let bang = Bang NoSourceUnpackedness NoSourceStrictness
  cName <- newName $ "Data" <> name <> "Message"
  let cons = NormalC cName [(bang, ConT tName)]
  -- partial for parser map
  Just fmapName <- lookupValueName "fmap"
  Just compose <- lookupValueName "."
  Just mpName <- lookupValueName "messageParserByFieldNumbers"
  -- (MesgNumFooBar, fmap FooBarConstructor . messageParserByFieldNumbers )
  let tup = TupE [ConE messageNumName, UInfixE (AppE (VarE fmapName) (ConE cName)) (VarE compose) (VarE mpName)]
  return $ (lens ++ inst, tup, cons)


partitionRows :: [MessageRow] -> [[MessageRow]]
partitionRows [] = []
partitionRows rows =
  let (x:xs) = dropWhile (\row -> isBlank row || isSectionRow row) rows
      (records, remainder) = span isValRow xs
  in (x:records):(partitionRows remainder)
  where
     isBlank (a, b, c, d, e, f, g) = all ((==) T.empty) [a, b, c, d, e, f, g]
     isTypeRow (typName, _, _, _, _, _, _) = typName /= T.empty
     isValRow (_, idx, recName, baseType, _, _, _) = recName /= T.empty && baseType /= T.empty
     isSectionRow (_, _, c, section, _, _, _) = section /= T.empty && c == T.empty

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
  
