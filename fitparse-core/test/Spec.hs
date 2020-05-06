module Main where
import Test.Hspec
import Conduit
import qualified Data.Conduit.Combinators as DC
import Fitparse
import Model
import TH.ProfileMessages

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
      let sample :: ConduitT () DataMessageContents M ()
          sample =  (sourceFile "test/test.fit".| readFitFile  .| dataMessageFilter .| DC.take 22 )
      ls <-runResourceT $  runConduit (sample .| sinkList)
      mapM_ print ls
      


