module Main where
import Test.Hspec
import Conduit
import qualified Data.Conduit.Combinators as DC
import Fitparse
import Fitparse.Model
import Fitparse.TH.ProfileMessages

import THSpec


--describe "Relatable" $ do
--  describe "contains" $ do
--    it "should relate a polygon containing a point " $ contains poly1 point1 `shouldBe` True

  
main :: IO ()
main = hspec $ do
  -- spec
  describe "Fitparse" $ do
    it "asd" $ do
      --1 `shouldBe` 1
      --return ()
      let sample :: ConduitT () DataMessageContents (ResourceT IO) ()
          sample =  (sourceFile "test/test.fit".| readFitFile  .| dataMessageFilter)
      ls <-runResourceT $  runConduit (sample .| sinkList)
      mapM_ print ls
      


