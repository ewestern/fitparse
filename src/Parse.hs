{-# LANGUAGE LambdaCase #-}

module Parse where

import Data.Bits
import Data.Int
import Data.Serialize.Get
import Data.Serialize.IEEE754
import Data.Vector (Vector)
import Data.Word
import qualified Data.Map as M
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Vector as V
import Control.Monad

import Model
import Signed

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
  globalMessageNumber :: Int,
  numFields :: Int,
  fieldDefinitionContents :: Vector FieldDefinitionContents,
  numDeveloperFields :: Maybe Int,
  developerFieldContents :: Maybe (Vector DeveloperFieldContents)
-}

getMessage :: M.Map Int DefinitionMessageContent -> Get (Either String Message)
getMessage defs = do
  header <- getHeader
  case header of
    NormalHeader DefinitionMessageType lmt ->
      Right . DefinitionMessage header <$> getDefinitionMessageContent
    NormalHeader DataMessageType lmt -> case M.lookup lmt defs of
      Just (DefinitionMessageContent messageNum numFields fdc ndf dfc) -> 
          Right . DataMessage header
        <$> parseDataMessageContents (fieldDefinitionContents dmc)
      Nothing -> pure $ Left ("Could not find local message type: " <> show lmt)
    CompressedTimestampHeader lmt to -> case M.lookup lmt defs of
      Just dmc -> Right . DataMessage header
        <$> parseDataMessageContents (fieldDefinitionContents dmc)
      Nothing -> pure $ Left ("Could not find local message type: " <> show lmt)



getDefMessageContent ::Either String Message -> Maybe (Int, DefinitionMessageContent)
getDefMessageContent (Right (DefinitionMessage (NormalHeader mt lmt) content)) = Just (lmt, content)
getDefMessageContent _ = Nothing

getHeader :: Get MessageHeader
getHeader = do
  i <- getWord8
  if testBit i 7
    then pure $ getTimestampHeader i
    else pure $ getNormalHeader i


getNormalHeader :: Word8 -> MessageHeader  
getNormalHeader i = 
  let messageType = if testBit i 6 then DefinitionMessageType else DataMessageType
      localNum = fromIntegral $ shiftR i 4
  in NormalHeader messageType localNum
  
  

timestampMask :: Word8
timestampMask = foldl setBit zeroBits [0..4]

timestampLocalMessageTypeMask :: Word8
timestampLocalMessageTypeMask = foldl setBit zeroBits [6,7]



getTimestampHeader :: Word8 -> MessageHeader
getTimestampHeader i =
  let off = i .&. timestampMask
      localNum = shiftR i 1 .&. timestampLocalMessageTypeMask
  in  CompressedTimestampHeader (fromIntegral localNum) (fromIntegral off)

getDefinitionMessageContent :: Get DefinitionMessageContent 
getDefinitionMessageContent = do
  _ <- getWord8
  arch <- getWord8
  if (fromIntegral arch) == 1 
    then getDefinitionMessageContentBE
    else getDefinitionMessageContentLE
  
-- TODO return to developer fields
getDefinitionMessageContentBE :: Get DefinitionMessageContent 
getDefinitionMessageContentBE = do
  gmn <- fromIntegral <$> getWord16be
  numField <- fromIntegral <$> getWord8
  defCon <- V.replicateM (fromIntegral numField) getFieldDefinitionContents
  pure $ DefinitionMessageContent gmn numField defCon Nothing Nothing


getDefinitionMessageContentLE :: Get DefinitionMessageContent 
getDefinitionMessageContentLE = do
  gmn <- fromIntegral <$> getWord16le
  numField <- fromIntegral <$> getWord8
  defCon <- V.replicateM (fromIntegral numField) getFieldDefinitionContents
  pure $ DefinitionMessageContent gmn numField defCon Nothing Nothing


nullByte :: Word8
nullByte = 0

getNulTerminatedByteString :: Get ByteString
getNulTerminatedByteString = fmap (BSL.pack . takeWhile (/= nullByte)) $ getListOf getWord8

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
  

parseDataMessageBaseValue :: BaseType -> Get (Maybe BaseValue)
parseDataMessageBaseValue bt = case bt of
  BTEnum    -> (Just . BVEnum . fromIntegral) <$> getWord8
  BTSint8   -> (Just . BVSint8 . signed) <$> getWord8
  BTUint8   -> (Just . BVUint8 . unsigned) <$> getWord8
  BTSint16  -> (Just . BVSint16 . signed) <$> getWord16le
  BTUint16  -> (Just . BVUint16 . unsigned) <$> getWord16le
  BTSint32  -> (Just . BVSint32 . signed) <$> getWord32le
  BTUint32  -> (Just . BVUint32 . unsigned) <$> getWord32le
  BTString  -> (Just . BVString) <$> getNulTerminatedByteString
  BTFloat32 -> (Just . BVFloat32) <$> getFloat32le
  BTFloat64 -> (Just . BVFloat64) <$> getFloat64le
  BTUint8z  -> pure Nothing
  BTUint16z -> pure Nothing
  BTUint32z -> pure Nothing
  BTByte    -> (Just . BVByte) <$> getWord8
  BTSint64  -> (Just . BVSint64 . signed) <$> getWord64le
  BTUint64  -> (Just . BVUint64 . unsigned) <$> getWord64le
  BTUint64z -> pure Nothing
  _         -> pure Nothing


parseDataMessageContent :: FieldDefinitionContents -> Get (Vector (Maybe BaseValue))
parseDataMessageContent (FieldDefinitionContents _ size (Just bt)) = 
  V.replicateM (numElements size bt) (parseDataMessageBaseValue bt)
parseDataMessageContent _ = pure V.empty


parseDataMessageContents :: V.Vector FieldDefinitionContents -> Get DataMessageContents
parseDataMessageContents = V.mapM parseDataMessageContent 

getFieldDefinitionContents :: Get FieldDefinitionContents
getFieldDefinitionContents = do
    fdn <- fromIntegral <$> getWord8
    fieldSize <- fromIntegral <$> getWord8
    baseType <- getBaseType <$> getWord8
    return $ FieldDefinitionContents fdn fieldSize baseType

