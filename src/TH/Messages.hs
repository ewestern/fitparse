{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}


module TH.Messages where

import qualified Data.Text as T
import Data.Text.Read
import Data.Text (Text)
import Data.Char (toLower)
import Data.Csv hiding (Name)
import qualified Data.ByteString.Lazy.Char8 as BSL
import qualified Data.Vector as V
import Data.Serialize.Get
import Control.Monad (join)

import Language.Haskell.TH
import Language.Haskell.TH.Quote
import Language.Haskell.TH.Syntax

import TH.Types (typeifyName, parseBaseType, getInternalTypeName)

import FitType
import BaseTypes

type RawMessageRow = [Text]
type MessageRow = (Text, Text, Text, Text)

dropColumns :: RawMessageRow -> MessageRow 
dropColumns (a:b:c:d:_) = (a, b, c, d)


recordName :: String -> String
recordName (x:xs) = (toLower x):xs
recordName [] = error "Shouldn't happen."


makeRecord :: String -> MessageRow -> Q VarBangType
makeRecord typName (_, _, recName, typ)  = do
    innerTypeName <- lookupType typ

    Just maybe <- lookupType "Maybe"
    let bang = Bang NoSourceUnpackedness NoSourceStrictness
    recordName <- newName $ recordName $ typName <> (typeifyName recName)
    case innerTypeName of
      Just tn -> return (recordName, bang, AppT (ConT maybe) (ConT tn))
      Nothing -> fail $ "Could not recognize type name: " <> (T.unpack typ)

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
    

--makeFitInstance :: BaseType -> Name -> Name -> Q [Dec]
--makeFitInstance baseType messageName toFuncName  = do
--  let typeNameString = nameBase messageName
--  fitFuncName <- newName $ "fit" <> typeNameString
--  Just parserName <- getParser baseType
--  ex <- [e|do
--    val <- $(varE parserName) 
--    let maybeVal = $(varE toFuncName) val
--    maybeToMonadFail ("Could not recognize value" <> (show val)) maybeVal|]
--  let fitParserFunc = FunD fitFuncName [Clause [] (NormalB ex) []]
--  Just cn <- lookupTypeName "FitType"
--  Just messageParser <- lookupValueName "typeParser"
--  let func = FunD messageParser [Clause [] (NormalB $ VarE fitFuncName) []]
--  let inst =  InstanceD Nothing [] (AppT (ConT cn) (ConT messageName)) [func]
--  return $ [fitParserFunc, inst]

  {-


instance FitMessage Foo where
  messageParser = Foo
    <$> lookaheadOpt typeParser
    <*> lookaheadOpt typeParser
 -}

parseTypeOpt :: FitType a => Get (Maybe a)
parseTypeOpt = lookaheadOpt typeParser

lookaheadOpt :: Get a -> Get (Maybe a)
lookaheadOpt get = lookAheadM $ fmap Just get


makeFitMessageInstance :: Name -> Name -> Int -> Q [Dec]
makeFitMessageInstance messageName constructorName numRecords = do
  Just fmapName <- lookupValueName "<$>"
  Just appName <- lookupValueName "<*>"
  Just tp <- lookupValueName "typeParser"
  Just mp <- lookupValueName "messageParser"
  Just lookahead <- lookupValueName "lookaheadOpt"
  Just cn <- lookupTypeName "FitMessage"
  let tpExpr = AppE (VarE lookahead) (VarE tp)
  let exp = foldl (\exp n -> UInfixE exp (VarE (if n == 1 then fmapName else appName)) tpExpr )  (ConE constructorName) $ [1..numRecords]
  let func = FunD mp [Clause [] (NormalB exp) []]
  let inst =  InstanceD Nothing [] (AppT (ConT cn) (ConT messageName)) [func]
  return [inst]

  


makeMessageType :: [MessageRow] -> Q ([Dec], Exp, Con)
makeMessageType [] = fail "Should not get empty MessageRow list."
makeMessageType ((messageName, _, _, _):xs) = do
  let name = typeifyName messageName
  Just messageNumName <- lookupValueName $ "MesgNum" <>  name
  tName <- newName $ name <> "Message"
  mName <- newName $ name <> "Message"
  records <- traverse (makeRecord name) xs
  Just showName <- lookupTypeName "Show"
  Just eqName <- lookupTypeName "Eq"
  let datadec = DataD [] tName [] Nothing [RecC mName records] [DerivClause Nothing [ConT showName, ConT eqName]]
  inst <- makeFitMessageInstance tName mName (length records)
  -- TopLevel constructor name
  let bang = Bang NoSourceUnpackedness NoSourceStrictness
  cName <- newName $ "Data" <> name <> "Message"
  let cons = NormalC cName [(bang, ConT tName)]
  -- partial for parser map
  Just fmapName <- lookupValueName "<$>"
  Just mpName <- lookupValueName "messageParser"
  let tup = TupE [ConE messageNumName, UInfixE (ConE cName) (VarE fmapName) (VarE mpName)]
  return $ ([datadec] ++ inst, tup, cons)

partitionRows :: [MessageRow] -> [[MessageRow]]
partitionRows [] = []
partitionRows rows =
  let (x:xs) = dropWhile (\row -> isBlank row || isSectionRow row) rows
      (records, remainder) = span isValRow xs
  in (x:records):(partitionRows remainder)
  where
     isBlank (a, b, c, d) = all ((==) T.empty) [a, b, c, d]
     isTypeRow (typName, _, _, _) = typName /= T.empty
     isValRow (_, idx, recName, baseType) = recName /= T.empty && baseType /= T.empty
     isSectionRow (_, _, c, section) = section /= T.empty && c == T.empty

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
  
