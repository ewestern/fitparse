module Main where

import Data.Int
import Data.Word
import Data.Time.Clock
import Test.Hspec
import Fitparse
import Conduit
import Control.Lens hiding (from)
import Fitparse.TH.Messages

import Fitparse
import Fitparse.Query
import Fitparse.Util






-- TODO: need a way to parse a value as an integral (e.g., Word32) but then store (or possibly retrieve) its scaled version as a Float.

selectQuery :: ConduitT () (Maybe Double, Maybe Double, Maybe Word8, Maybe UTCTime) (ResourceT IO) ()
selectQuery = (select ( semiToDegrees $ _DataRecordMessage.recordPositionLong
                , semiToDegrees $ _DataRecordMessage.recordPositionLat
                , _DataRecordMessage.recordHeartRate
                , toUTC $ _DataRecordMessage.recordTimestamp
                ))  `from` (sourceFile "test/test_2.fit") `where_` (_1 `lte` 0)



-- thing :: ConduitT (Maybe Double, Maybe Double, Maybe Word8, Maybe UTCTime) () (ResourceT IO) ()
-- thing = do
--   tup <- await
--   case tup of 
--     Just (Just lng, Just lat, Just hr, Just ts) -> do
--       modify 


  


main :: IO ()
main = hspec $ do
  -- spec
  describe "Fitparse" $ do
    it "asd" $ do
      let s = selectQuery .| sinkList
      ls <- runResourceT $  runConduit s
      mapM_ print ls
