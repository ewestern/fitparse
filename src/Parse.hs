{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Parse where

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

import Model
import TH.ProfileMessages
import FitType
import TH.ProfileTypes
import Signed
import BaseTypes


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
    NormalHeader DefinitionMessageType lmt hasDev ->
      Right . DefinitionMessage header <$> (getDefinitionMessageContent hasDev)

    NormalHeader DataMessageType lmt hasDev -> case M.lookup lmt defs of
      Just (header, definition) -> getMessageWithFieldDefinitions header definition
      Nothing -> pure $ Left ("Could not find local message type: " <> show lmt <> (show $ M.keys defs))
    CompressedTimestampHeader lmt to -> case M.lookup lmt defs of
      Just (header, definition) -> getMessageWithFieldDefinitions header definition
      Nothing -> pure $ Left ("Could not find local message type: " <> show lmt <> (show $ M.keys defs))

getMessageWithFieldDefinitions :: MessageHeader -> DefinitionMessageContent -> Get (Either String Message)
getMessageWithFieldDefinitions header@(NormalHeader _ lmc _) (DefinitionMessageContent gmn numFields fdc ndf df)
  = do byteString <- getBytes $ sum $ fmap fieldSize fdc
       let vec = fmap fieldDefinitionNumber fdc
       case join $ traverse (flip M.lookup parserMap) gmn  of
         Just dmcParser -> return $ fmap (DataMessage header) $ runGet (dmcParser vec) byteString
         Nothing -> return $ Left $ "Couldn't find message num: " <> (show gmn)
    where
      getByteString :: V.Vector BS.ByteString -> M.Map Int Int -> Int -> Maybe BS.ByteString
      getByteString byteStrings indexMap i = do
        idx <- M.lookup i indexMap
        byteStrings V.!? idx

getHeaderForMap :: Either String Message -> Maybe (Int, (MessageHeader, DefinitionMessageContent))
getHeaderForMap (Right (DefinitionMessage header@(NormalHeader mt lmt _) defMessage)) 
  = Just (lmt, (header, defMessage))
getHeaderForMap _ = Nothing
  



-- getParserForMessage ::Either String Message -> Maybe (Int, Get DataMessageContents)
-- getParserForMessage (Right (DefinitionMessage (NormalHeader mt lmt _) (DefinitionMessageContent gmt _ _ _ _))) = 
--   case M.lookup gmt parserMap of
--     Just get -> Just (lmt, get)
--     _ -> Nothing
-- getParserForMessage _ = Nothing

getHeader :: Get MessageHeader
getHeader = do
  i <- getWord8
  if testBit i 7
    then pure $ getTimestampHeader i
    else pure $ getNormalHeader i


getNormalHeader :: Word8 -> MessageHeader  
getNormalHeader i = 
  let messageType = if testBit i 6 then DefinitionMessageType else DataMessageType
      localNum = fromIntegral $ shiftL i 4
  in NormalHeader messageType localNum (testBit i 5)
  
  

timestampMask :: Word8
timestampMask = foldl setBit zeroBits [0..4]

timestampLocalMessageTypeMask :: Word8
timestampLocalMessageTypeMask = foldl setBit zeroBits [6,7]



getTimestampHeader :: Word8 -> MessageHeader
getTimestampHeader i =
  let off = i .&. timestampMask
      localNum = shiftR i 1 .&. timestampLocalMessageTypeMask
  in  CompressedTimestampHeader (fromIntegral localNum) (fromIntegral off)

getDefinitionMessageContent :: Bool -> Get DefinitionMessageContent 
getDefinitionMessageContent hasDev = do
  _ <- getWord8
  arch <- getWord8
  if (fromIntegral arch) == 1 
     -- TODO: Come back to handle ENdianness
    then getDefinitionMessageContentLE hasDev
    else getDefinitionMessageContentLE hasDev
  

getDefinitionMessageContentLE :: Bool -> Get DefinitionMessageContent 
getDefinitionMessageContentLE hasDev = do
  gmn <- fmap toMesgNum getWord16le
  numField <- fromIntegral <$> getWord8
  defCon <- V.replicateM (fromIntegral numField) getFieldDefinitionContents
  maybeDevFields <- if hasDev 
     then do
       numFields <- getIntByte
       fieldDefs <- V.replicateM (fromIntegral numFields) getDevFieldDef
       return $ Just (numFields, fieldDefs)
      else return Nothing
  pure $ DefinitionMessageContent gmn numField defCon (fmap fst maybeDevFields) (fmap snd maybeDevFields) 
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


