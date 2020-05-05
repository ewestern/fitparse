{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module FitType where

import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy.Char8 as BSC
import qualified Data.ByteString.Lazy as BSL
import Data.Word
import Data.Int
import Data.Serialize.Get
import Data.Serialize.IEEE754
import Data.Proxy
import BaseTypes

-- TODO: deal with endianness!!!!
class FitType a where
  typeParser :: Get a

instance FitType Word8 where
  typeParser = getWord8

instance FitType Word16 where
  typeParser = getWord16le

instance FitType Word32 where
  typeParser = getWord32le

instance FitType Word64 where
  typeParser = getWord64le

instance FitType Int8 where
  typeParser = getInt8

instance FitType Int16 where
  typeParser = getInt16le

instance FitType Int32 where
  typeParser = getInt32le

instance FitType Int64 where
  typeParser = getInt64le

nullByte :: Word8
nullByte = 0

getNulTerminatedByteString :: Get ByteString
getNulTerminatedByteString = fmap (BSL.pack . takeWhile (/= nullByte)) $ getListOf getWord8


instance FitType ByteString where
  typeParser = getNulTerminatedByteString

instance FitType Float where
  typeParser = getFloat32le

instance FitType Double where
  typeParser = getFloat64le

instance FitType String where
  typeParser = BSC.unpack <$> typeParser

instance FitType Bool where
  typeParser = fmap ((/=) 0) getWord8
