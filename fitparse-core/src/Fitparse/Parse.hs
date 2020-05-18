{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Fitparse.Parse where

import Data.Bits
import Data.Int
import Data.Serialize.Get
import Data.Serialize.IEEE754
import Data.Vector (Vector)
import Data.Maybe
import qualified Data.Map as M
import Data.Word
import qualified Data.Map as M
import qualified Data.ByteString as BS
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Vector as V
import Control.Monad
import Debug.Trace

import Fitparse.Model
import Fitparse.TH.ProfileMessages
import Fitparse.FitType
import Fitparse.TH.ProfileTypes
import Fitparse.BaseTypes
import Fitparse.FitMessage


getGlobalHeader :: Get GlobalHeader
getGlobalHeader = do
  _ <- getWord8
  protocolVersion <- fromIntegral <$> getWord8
  profileVersion <- fromIntegral <$> getWord16le
  dataSize <- fromIntegral <$> getWord32le
  _ <- getWord32le
  crc <- fromIntegral <$> getWord16le
  pure $ GlobalHeader protocolVersion profileVersion dataSize crc
  
{-
 Definition Message: In a definition message, the local message type is assigned to a Global FIT Message Number (mesg_num) relating the local messages to their respective FIT messages
Data Message: The local message type associates a data message to its respective definition message, and hence, its' global FIT message. A data message will follow the format as specified in its definition message of matching local message type.
 -}

getMessage :: M.Map Int (MessageHeader, DefinitionMessageContent) -> Get (Either String Message)
getMessage defs = do
  header <- getHeader
  case header of
    NormalHeader DefinitionMessageType lmt hasDev -> do
      eitherDMC <- getDefinitionMessageContent hasDev
      pure $ DefinitionMessage header <$> eitherDMC
    NormalHeader DataMessageType lmt hasDev -> matchDataMessage lmt hasDev header
    CompressedTimestampHeader lmt to -> matchDataMessage lmt hasDev header

    where
      matchDataMessage lmt hasDev head = case M.lookup lmt defs of
        Just (header, definition) -> getMessageWithFieldDefinitions header definition head
        Nothing -> pure $ Left ("Could not find local message type: " <> show lmt <> ". Header = " <> (show head))


getMessageWithFieldDefinitions :: MessageHeader -> DefinitionMessageContent -> MessageHeader -> Get (Either String Message)
getMessageWithFieldDefinitions definitionHeader@(NormalHeader _ lmc _) def@(DefinitionMessageContent gmn numFields fdc ndf df) dataMessageHeader
  = do byteString <- getBytes $ sum $ fmap fieldSize fdc
       case gmn of
         Just MesgNumRecord -> do
            let unstructured = runGet (getUnstructuredContent def) byteString
            traceShowM unstructured
         _ -> pure ()
       case join $ traverse (flip M.lookup parserMap) gmn  of
         Just dmcParser -> pure $ fmap (DataMessage dataMessageHeader) $ runGet (dmcParser fdc) byteString
         Nothing -> pure $ fmap (UnstructuredDataMessage dataMessageHeader) $ runGet (getUnstructuredContent def) byteString
    where
      getByteString :: V.Vector BS.ByteString -> M.Map Int Int -> Int -> Maybe BS.ByteString
      getByteString byteStrings indexMap i = do
        idx <- M.lookup i indexMap
        byteStrings V.!? idx

getUnstructuredContent :: DefinitionMessageContent -> Get UnstructuredContent
getUnstructuredContent (DefinitionMessageContent gmn numFields fieldDefs numDevFields devFieldContents) = 
  traverse getField fieldDefs
  where 
    getField (FieldDefinitionContents defNum fieldSize (Just bt)) = 
      V.singleton <$> getBaseValueByType bt

getHeaderForMap :: Either String Message -> Maybe (Int, (MessageHeader, DefinitionMessageContent))
getHeaderForMap (Right (DefinitionMessage header@(NormalHeader mt lmt _) defMessage)) 
  = Just (lmt, (header, defMessage))
getHeaderForMap _ = Nothing
  

getHeader :: Get MessageHeader
getHeader = do
  i <- getWord8
  if testBit i 7
    then pure $ getTimestampHeader i
    else pure $ getNormalHeader i


localMessageNumberMask :: Word8
localMessageNumberMask = foldl setBit zeroBits [0..3]

getNormalHeader :: Word8 -> MessageHeader  
getNormalHeader i = 
  let messageType = if testBit i 6 then DefinitionMessageType else DataMessageType
      localNum = fromIntegral $ i .&. localMessageNumberMask
  in NormalHeader messageType localNum (testBit i 5)
  
  

timestampMask :: Word8
timestampMask = foldl setBit zeroBits [0..4]

timestampLocalMessageTypeMask :: Word8
timestampLocalMessageTypeMask = foldl setBit zeroBits [0,1]



getTimestampHeader :: Word8 -> MessageHeader
getTimestampHeader i =
  let off = i .&. timestampMask
      localNum = (shiftR i 5) .&. timestampLocalMessageTypeMask
  in  CompressedTimestampHeader (fromIntegral localNum) (fromIntegral off)

getDefinitionMessageContent :: Bool -> Get (Either String DefinitionMessageContent)
getDefinitionMessageContent hasDev = do
  _ <- getWord8
  arch <- getWord8
  if (fromIntegral arch) == 1 
     -- TODO: Come back to handle ENdianness
    then getDefinitionMessageContentLE hasDev
    else getDefinitionMessageContentLE hasDev
  

getDefinitionMessageContentLE :: Bool -> Get (Either String DefinitionMessageContent)
getDefinitionMessageContentLE hasDev = do
  gmn <- getWord16le
  numField <- fromIntegral <$> getWord8
  defCon <- V.replicateM (fromIntegral numField) getFieldDefinitionContents
  maybeDevFields <- if hasDev 
     then do
       numFields <- getIntByte
       fieldDefs <- V.replicateM (fromIntegral numFields) getDevFieldDef
       return $ Just (numFields, fieldDefs)
      else return Nothing
  pure $ Right $ DefinitionMessageContent (toMesgNum gmn) numField defCon (fmap fst maybeDevFields) (fmap snd maybeDevFields) 

    where
      getDevFieldDef :: Get DeveloperFieldContents
      getDevFieldDef = DeveloperFieldContents <$> getIntByte <*> getIntByte <*> getIntByte
      getIntByte :: Get Int
      getIntByte = fromIntegral <$> getInt8

  

getBaseType :: Word8 -> Maybe BaseType
getBaseType = \case
  0x00 -> Just BTEnum
  0x01 -> Just BTSint8
  0x02 -> Just BTUint8
  0x83 -> Just BTUint16
  0x84 -> Just BTSint16
  0x85 -> Just BTUint32
  0x86 -> Just BTSint32
  0x07 -> Just BTString
  0x88 -> Just BTFloat32
  0x89 -> Just BTFloat64
  0x0A -> Just BTUint8z
  0x8B -> Just BTUint16z
  0x8C -> Just BTUint32z
  0x0D -> Just BTByte
  0x8E -> Just BTSint64
  0x8F -> Just BTUint64
  0x90 -> Just BTUint64z
  _ -> Nothing
  

getFieldDefinitionContents :: Get FieldDefinitionContents
getFieldDefinitionContents = do
    fdn <- fromIntegral <$> getWord8
    fieldSize <- fromIntegral <$> getWord8
    baseType <- getBaseType <$> getWord8
    return $ FieldDefinitionContents fdn fieldSize baseType


getBaseValueByType :: BaseType -> Get BaseValue
getBaseValueByType = \case
  BTEnum ->     BVEnum <$> typeParser
  BTSint8 ->    BVSint8 <$> typeParser
  BTUint8 ->    BVUint8 <$> typeParser
  BTSint16 ->   BVSint16 <$> typeParser
  BTUint16 ->   BVUint16 <$> typeParser 
  BTSint32 ->   BVSint32 <$> typeParser
  BTUint32 ->   BVUint32 <$> typeParser
  BTString ->   BVString <$> typeParser
  BTFloat32 ->  BVFloat32 <$> typeParser
  BTFloat64 ->  BVFloat64 <$> typeParser
  BTUint8z ->   BVUint8z <$> typeParser
  BTUint16z ->  BVUint16z <$> typeParser
  BTUint32z ->  BVUint32z <$> typeParser
  BTByte ->     BVByte <$> typeParser
  BTSint64 ->   BVSint64 <$> typeParser
  BTUint64 ->   BVUint64 <$> typeParser
  BTUint64z ->  BVUint64z <$> typeParser

