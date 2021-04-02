module Main where

import Data.Int
import Data.Word
import Data.Time.Clock
import Test.Hspec
import Conduit
import Control.Lens hiding (from)
import Fitparse.TH.Messages

import Fitparse
import Fitparse.Query
import Fitparse.Util


-- TODO: need a way to parse a value as an integral (e.g., Word32) but then store (or possibly retrieve) its scaled version as a Float.

selectQuery = select (semiToDegrees $ _DataRecordMessage.recordPositionLong
                     , semiToDegrees $ _DataRecordMessage.recordPositionLat
                      )
                `from` (sourceFile "test/test_2.fit") 
                `where_` (_1 `gt` (-105.05))
  
main :: IO ()
main = hspec $ do
  -- spec
  describe "Fitparse" $ do
    it "filtering works properly" $ do
      ls <- runResourceT $  runConduit $ selectQuery .| sinkList
      length ls `shouldBe` 332
