{-# LANGUAGE LambdaCase #-}
module Model where

import Data.Int
import Data.Word
import Data.Vector (Vector)
import qualified Data.Vector as V
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as BSL
import Signed
import FileTypes

--- 
data GlobalHeader  = GlobalHeader {
    protocolVersion :: Int
  , profileVersion :: Int
  , dataSize:: Int
  , crc :: Int
} deriving (Eq, Ord, Show)



-- parseDataMessageContents :: V.Vector FieldDefinitionContents -> Get DataMessageContents
data Message  
    = DataMessage MessageHeader DataMessageContents
    | DefinitionMessage MessageHeader DefinitionMessageContent
    | GlobalHeaderMessage GlobalHeader 
  deriving (Eq, Ord, Show)


--
-- Message Header
--

data MessageHeader
  = NormalHeader {
    messageType :: MessageType,
    localMessageTypeNormal :: Int
  }
  | CompressedTimestampHeader {
    localMessageTypeCompressed :: Int,
    timeOffset :: Int
  }
  deriving (Eq, Ord, Show)

data MessageType 
  = DefinitionMessageType
  | DataMessageType
  deriving (Eq, Ord, Show)

--

---
-- Definition Message
---

data DefinitionMessageContent = DefinitionMessageContent {
  globalMessageNumber :: Int,
  numFields :: Int,
  fieldDefinitionContents :: Vector FieldDefinitionContents,
  numDeveloperFields :: Maybe Int,
  developerFieldContents :: Maybe (Vector DeveloperFieldContents)
}
  deriving (Eq, Ord, Show)

--
-- Data Message Contents
--

type DataMessageContents = V.Vector (V.Vector (Maybe BaseValue))

data FitMessage
  = FileIdMessage {
    fileType :: FileType
  , manufacturer :: Word16
  , product :: Word16
  , serial :: Word32
  , timeCreated :: Maybe Word32
  , number :: Maybe Word16
  } | SoftwareMessage {
    messageIndex :: Maybe Word16
  , version :: Maybe Word16
  , partNumber :: Maybe ByteString
  } | CapabilitiesMessage {
    languages :: Maybe Word16
  }
  


data BaseType
  = BTEnum
  | BTSint8
  | BTUint8
  | BTSint16
  | BTUint16
  | BTSint32
  | BTUint32
  | BTString
  | BTFloat32
  | BTFloat64
  | BTUint8z
  | BTUint16z
  | BTUint32z
  | BTByte
  | BTSint64
  | BTUint64
  | BTUint64z
  deriving (Eq, Ord, Show)

data BaseValue
  = BVEnum Int
  | BVSint8 (Signed Int8)
  | BVUint8 (Unsigned Word8)
  | BVSint16 (Signed Int16)
  | BVUint16 (Unsigned Word16)
  | BVSint32 (Signed Int32)
  | BVUint32 (Unsigned Word32)
  | BVString ByteString
  | BVFloat32 Float
  | BVFloat64 Double
  | BVUint8z (Unsigned Word8)
  | BVUint16z (Unsigned Word16)
  | BVUint32z (Unsigned Word32)
  | BVByte Word8
  | BVSint64 (Signed Int64)
  | BVUint64 (Unsigned Int64)
  | BVUint64z (Unsigned Int64)
  deriving (Eq, Ord, Show)

numElements :: Int -> BaseType -> Int
numElements size = \case
  BTEnum -> size
  BTSint8 -> size
  BTUint8 -> size
  BTSint16 -> div size 2
  BTUint16 -> div size 2
  BTSint32 -> div size 4
  BTUint32 -> div size 4
  BTString -> size
  BTFloat32 -> div size 4
  BTFloat64 -> div size 4
  BTUint8z -> size
  BTUint16z -> div size 2
  BTUint32z -> div size 4
  BTByte -> size
  BTSint64 -> div size 8
  BTUint64 -> div size 8
  BTUint64z -> div size 8


data FieldDefinitionContents
  = FieldDefinitionContents {
  fieldDefinitionNumber :: Int,
  fieldSize :: Int, -- in bytes
  baseType :: Maybe BaseType
}
  deriving (Eq, Ord, Show)



data DeveloperFieldContents
  = DeveloperFieldContents ()
  deriving (Eq, Ord, Show)


 
