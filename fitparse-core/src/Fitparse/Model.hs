{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}
module Fitparse.Model where

import Data.Int
import Data.Word
import Data.Vector (Vector)
import qualified Data.Vector as V
import Data.ByteString.Lazy (ByteString)
import Control.Monad
import qualified Data.ByteString.Lazy as BSL
import Control.Lens

import Fitparse.BaseTypes
import Fitparse.TH.ProfileMessages
import Fitparse.TH.ProfileTypes

--- 
data GlobalHeader  = GlobalHeader {
    protocolVersion :: Int
  , profileVersion :: Int
  , dataSize:: Int
  , crc :: Int
} deriving (Eq, Ord, Show)

data Message  
    = DataMessage MessageHeader DataMessageContents
    | UnstructuredDataMessage MessageHeader UnstructuredContent
    | DefinitionMessage MessageHeader DefinitionMessageContent
    | GlobalHeaderMessage GlobalHeader 
  deriving (Eq, Show)


type UnstructuredContent = Vector (Vector BaseValue)

--
-- Message Header
--

data MessageHeader
  = NormalHeader 
    { messageType :: MessageType
    , localMessageTypeNormal :: Int
    , hasDev :: Bool }
  | CompressedTimestampHeader 
    { localMessageTypeCompressed :: Int,
      timeOffset :: Int }
  deriving (Eq, Ord, Show)

data MessageType 
  = DefinitionMessageType
  | DataMessageType
  deriving (Eq, Ord, Show)


getLocalMessageType :: MessageHeader -> Int
getLocalMessageType (NormalHeader _ lmt _) = lmt
getLocalMessageType (CompressedTimestampHeader lmt _) = lmt


---
-- Definition Message
---
data DefinitionMessageContent = DefinitionMessageContent {
  globalMessageNumber :: Maybe MesgNum, -- this shouldn't be optional, but :shrug:
  numFields :: Int,
  fieldDefinitionContents :: Vector FieldDefinitionContents,
  numDeveloperFields :: Maybe Int,
  developerFieldContents :: Maybe (Vector DeveloperFieldContents)
}
  deriving (Eq, Ord, Show)

--
-- Data Message Contents
--

data FieldDefinitionContents
  = FieldDefinitionContents {
  fieldDefinitionNumber :: Int,
  fieldSize :: Int, -- in bytes
  baseType :: Maybe BaseType
}
  deriving (Eq, Ord, Show)

data DeveloperFieldContents
  = DeveloperFieldContents 
    { fieldNumber :: Int
    , devFieldSize :: Int
    , devDataIndex :: Int }
  deriving (Eq, Ord, Show)

makePrisms ''Message
