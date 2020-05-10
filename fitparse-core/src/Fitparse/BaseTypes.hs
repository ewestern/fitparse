{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DeriveDataTypeable #-}

module Fitparse.BaseTypes where

import Data.Int
import Data.Word
import Data.Data
import Data.Typeable
import Data.ByteString.Lazy (ByteString)

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
  deriving (Eq, Ord, Show, Data, Typeable)

data BaseValue
  = BVEnum Word8
  | BVSint8 Int8
  | BVUint8 Word8
  | BVSint16 Int16
  | BVUint16 Word16
  | BVSint32 Int32
  | BVUint32 Word32
  | BVString ByteString
  | BVFloat32 Float
  | BVFloat64 Double
  | BVUint8z Word8
  | BVUint16z Word16
  | BVUint32z Word32
  | BVByte Word8
  | BVSint64 Int64
  | BVUint64 Word64
  | BVUint64z Word64
  deriving (Eq, Ord, Show, Data, Typeable)


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


type DateTime = Word32
type LocalDateTime = Word32
type LocaltimeIntoDay = Word32
type TimeIntoDay = Word32
type LocalDeviceType = Word8

