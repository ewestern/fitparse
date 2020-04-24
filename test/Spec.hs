module Main where
import Test.Hspec
import Conduit
import Control.Monad.Trans.Resource
import qualified Data.Conduit.Combinators as DC
import Fitparse
import Model


--describe "Relatable" $ do
--  describe "contains" $ do
--    it "should relate a polygon containing a point " $ contains poly1 point1 `shouldBe` True

  
main :: IO ()
main = hspec $ do
  describe "Fitparse" $ do
    it "asd" $ do
      let sample :: ConduitT () (Either String Message) M ()
          sample =  (sourceFile "test/test.fit".| readFitFile  .| DC.take 10 )
      ls <-runResourceT $  runConduit (sample .| sinkList)
      mapM_ print ls
      


