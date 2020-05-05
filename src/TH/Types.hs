{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module TH.Types where

import qualified Data.Text as T
import Data.Either
import Data.Text.Read
import Data.Text (Text)
import Data.Csv hiding (Name)
import Control.Monad (join)
import qualified Data.ByteString.Lazy.Char8 as BSL
import qualified Data.Vector as V
import Data.Serialize.Get

import Language.Haskell.TH
import Language.Haskell.TH.Quote
import Language.Haskell.TH.Syntax

import FitType

import BaseTypes



type TypeRow = (Text, Text, Text, Text, Text)

typeifyName :: Text -> String
typeifyName = T.unpack . T.intercalate "".  fmap T.toTitle . T.splitOn "_"


parseBaseType :: Text -> Maybe BaseType
parseBaseType = \case
  "enum" -> Just BTEnum
  "sint8" -> Just BTSint8
  "uint8" -> Just BTUint8
  "sint16" -> Just BTSint16
  "uint16" -> Just BTUint16
  "sint32" -> Just BTSint32
  "uint32" -> Just BTUint32
  "string" -> Just BTString
  "float32" -> Just BTFloat32
  "float64" -> Just BTFloat64
  "uint8z" -> Just BTUint8z
  "uint16z" -> Just BTUint16z
  "uint32z" -> Just BTUint32z
  "byte" -> Just BTByte
  "sint64" -> Just BTSint64
  "uint64" -> Just BTUint64
  "uint64z" -> Just BTUint64z
  _  ->  Nothing


getValue :: BaseType -> Text -> Get a
getValue baseType val = undefined


parseValue :: BaseType -> Text -> Either String Lit
parseValue bt val = parser val
  where 
    errorIfRemainder ::  Either String (a, Text) -> Either String a
    errorIfRemainder = \case
      Right (v, rem) -> if rem /= T.empty 
                           then Left ("Could not parse: " <> T.unpack rem)
                            else Right v
      e -> fmap fst e

    decOrHex :: Integral a => Reader a
    decOrHex t = case hexadecimal t of
                   r@(Right _) -> r
                   _ -> decimal t

    extractIntValue :: Integral a => (a -> Lit) -> Text -> Either String Lit
    extractIntValue  f = fmap f . errorIfRemainder  . decOrHex

    extractFracValue :: Fractional a => (a -> Lit) -> Text -> Either String Lit
    extractFracValue  f = fmap f . fmap fst  . rational

    parser =  case bt of
      BTEnum ->     extractIntValue IntegerL
      BTSint8 ->    extractIntValue IntegerL
      BTUint8 ->    extractIntValue IntegerL
      BTSint16 ->   extractIntValue IntegerL
      BTUint16 ->   extractIntValue IntegerL
      BTSint32 ->   extractIntValue IntegerL
      BTUint32 ->   extractIntValue IntegerL
      BTString ->   const $ Left "Could not parse string type"
      BTFloat32 ->  extractFracValue FloatPrimL
      BTFloat64 ->  extractFracValue DoublePrimL
      BTUint8z ->   extractIntValue IntegerL
      BTUint16z ->  extractIntValue IntegerL
      BTUint32z ->  extractIntValue IntegerL
      BTByte ->     extractIntValue IntegerL
      BTSint64 ->   extractIntValue IntegerL
      BTUint64 ->   extractIntValue IntegerL
      BTUint64z ->  extractIntValue IntegerL
  

getInternalTypeName :: BaseType -> Q (Maybe Name)
getInternalTypeName = lookupTypeName . \case
      BTEnum ->     "Word8"
      BTSint8 ->    "Int8"
      BTUint8 ->    "Word8"
      BTSint16 ->   "Int16"
      BTUint16 ->   "Word16"
      BTSint32 ->   "Int32" 
      BTUint32 ->   "Word32"  
      BTString ->   "ByteString" 
      BTFloat32 ->  "Float" 
      BTFloat64 ->  "Double"
      BTUint8z ->   "Word8" 
      BTUint16z ->  "Word16"
      BTUint32z ->  "Word32"
      BTByte ->     "Word8"
      BTSint64 ->   "Int64"
      BTUint64 ->   "Word64"
      BTUint64z ->  "Word64"


getParser :: BaseType -> Q (Maybe Name)
getParser = lookupValueName . \case
      BTEnum ->     "getWord8"
      BTSint8 ->    "getInt8"
      BTUint8 ->    "getWord8"
      BTSint16 ->   "getInt16le"
      BTUint16 ->   "getWord16le"
      BTSint32 ->   "getInt32le" 
      BTUint32 ->   "getWord32le"  
      BTString ->   "getNulTerminatedByteString" 
      BTFloat32 ->  "getFloat32le" 
      BTFloat64 ->  "getFloat64le"
      BTUint8z ->   "getWord8" 
      BTUint16z ->  "getWord16le"
      BTUint32z ->  "getWord32le"
      BTByte ->     "getWord8"
      BTSint64 ->   "getInt64le"
      BTUint64 ->   "getWord64le"
      BTUint64z ->  "getWord64le"

makeConstructor :: String -> BaseType -> TypeRow -> Q (Con, Match)  
makeConstructor typName bt (_, _, var, val, _)  = do
    conName <- newName $ typName <> (typeifyName var)
    Just just <- lookupValueName "Just"
    case parseValue bt val of
      Right lit ->  do
        let con = NormalC conName []
            match = Match (LitP  lit) (NormalB $ AppE (ConE just) $ ConE conName) []
        return $ (con, match)
      Left e -> fail $ (e <> "Context: type name= " <> typName <> ". constructor name=" <> (T.unpack var) <> ". value=" <> (T.unpack val))


mkToFunc :: String -> BaseType -> [(Con, Match)] -> Q (Dec, Name)
mkToFunc typeName baseType constructorsAndMatches = do
  funcName <- newName $ "to" <> typeName
  varTyp <- newName "val"
  let matches = fmap snd constructorsAndMatches
  Just nothing <- lookupValueName "Nothing"
  let wildcard = Match WildP (NormalB $ ConE nothing) []
  let exp = CaseE (VarE varTyp) $ matches ++ [wildcard]
  let c = Clause [VarP varTyp] (NormalB exp ) []
  return $ (FunD funcName [c], funcName)

conToExp :: Con -> Q Exp
conToExp (NormalC name []) = return $ ConE name
conToExp _ = fail "Can only recognize empty constructors"

partitionRows :: [TypeRow] -> [[TypeRow]]
partitionRows [] = []
partitionRows rows =
   let (typeRow, tail) = span isTypeRow rows
       (valRows, remainder) = span isValRow tail
       first = typeRow ++ valRows
   in first:(partitionRows remainder)
    where
       isTypeRow (typName, baseType, _, _, _) = typName /= T.empty  && baseType /= T.empty
       isValRow (_, _, consName, baseValue, _) = consName /= T.empty && baseValue /= T.empty

makeFitParser :: String -> Name -> Q [Dec]
makeFitParser typeName toFuncName = do
  return $ []

maybeToMonadFail :: MonadFail m => String -> Maybe a -> m a
maybeToMonadFail str = \case
  Just v -> return v
  _ -> fail str


makeFitInstance :: BaseType -> Name -> Name -> Q [Dec]
makeFitInstance baseType messageName toFuncName  = do
  let typeNameString = nameBase messageName
  fitFuncName <- newName $ "fit" <> typeNameString
  Just parserName <- getParser baseType
  let contextMesg = litE $ StringL $ ". Context: parser for " <> typeNameString <> ". "
  ex <- [e|do
    val <- $(varE parserName) 
    let maybeVal = $(varE toFuncName) val
    maybeToMonadFail ("Could not recognize value " <> (show val) <> $(contextMesg)) maybeVal|]
  let fitParserFunc = FunD fitFuncName [Clause [] (NormalB ex) []]
  Just cn <- lookupTypeName "FitType"
  Just messageParser <- lookupValueName "typeParser"
  let func = FunD messageParser [Clause [] (NormalB $ VarE fitFuncName) []]
  let inst =  InstanceD Nothing [] (AppT (ConT cn) (ConT messageName)) [func]
  return $ [fitParserFunc, inst]


makeType :: [TypeRow] -> Q [Dec]
makeType [] = return []
makeType ((typeName, btype, _, _, _):xs) = do
  baseType <- case parseBaseType btype of
    Just bt -> return bt
    Nothing -> fail "Could not parse base type"
  let name = typeifyName typeName
  dtName <- newName name
  constructorsAndMatches <- traverse (makeConstructor name baseType) xs
  let constructors = fmap fst constructorsAndMatches
  Just showName <- lookupTypeName "Show"
  Just eqName <- lookupTypeName "Eq"
  Just ordName <- lookupTypeName "Ord"
  let datadec = DataD [] dtName [] Nothing constructors [DerivClause Nothing [ConT showName, ConT eqName, ConT ordName]]
  (toFunc, toFuncName) <- mkToFunc name  baseType constructorsAndMatches
  decs <- makeFitInstance baseType dtName toFuncName
  return $ [
      datadec
    , toFunc ] ++ decs

fittypeQ :: String -> Q [Dec]
fittypeQ str = 
 let rows :: Either String (V.Vector TypeRow) = decode NoHeader $ BSL.pack str
 in case rows of
    Right v -> fmap join $ traverse makeType $ partitionRows $ V.toList  v
    Left e -> fail e


fittype :: QuasiQuoter
fittype = quoteFile $ QuasiQuoter 
  { quoteExp = notHandled
  , quotePat = notHandled
  , quoteType = notHandled
  , quoteDec = fittypeQ
  }
  where notHandled things = error $
          things ++ " are not handled by the regex quasiquoter."
