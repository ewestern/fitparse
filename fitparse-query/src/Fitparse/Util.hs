{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE KindSignatures #-}

module Fitparse.Util (
    semiToDegrees
  , toUTC
) where

import Control.Applicative
import Control.Lens
import Data.Int
import Data.Word
import Data.Time.Clock
import Data.Time.Calendar

import Fitparse.Query
---

newtype Sphere = Sphere Double
type Coordinate = (Double, Double)

earthMean :: Sphere
earthMean = Sphere 6367450

haversine :: Sphere -> Coordinate -> Coordinate -> Double
haversine (Sphere s) (lat1, lng1) (lat2, lng2) = 
  let toRadians n = n * pi / 180
      dlat = toRadians (lat1 - lat2) / 2
      dlon = toRadians (lng1 - lng2) / 2
      cosr = cos . toRadians
      square x = x * x
      a = square (sin dlat) + cosr lat1 * cosr lat2 * square (sin dlon)
      c = 2 * atan2 (sqrt a) (sqrt (1 - a))
  in s * c

---

semiToDegrees :: (Contravariant f, Functor f) => Selecting f Int32 -> Selecting f Double
semiToDegrees v = v.toDegrees
  where
    toDegrees :: Getter (Maybe Int32) (Maybe Double)
    toDegrees = to $ fmap semiToDegrees'


factor :: Double
factor = 180 / (2^31)

semiToDegrees' :: Int32 -> Double
semiToDegrees' = ((*) factor) . fromIntegral

-- seconds since UTC 00:00 Dec 31 1989
epoch :: UTCTime
epoch = 
  let day = fromGregorian 1989 12 31
      diff = secondsToDiffTime 0
  in UTCTime day diff

toUTC' :: Word32 -> UTCTime
toUTC' w = addUTCTime (fromIntegral w) epoch

toUTC :: (Contravariant f, Functor f) => Selecting f Word32 -> Selecting f UTCTime
toUTC v = v.toTime
  where 
    toTime = to $ fmap toUTC'
    
