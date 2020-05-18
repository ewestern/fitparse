{-#LANGUAGE RankNTypes #-}
{-#LANGUAGE FlexibleContexts #-}
{-#LANGUAGE MultiParamTypeClasses #-}

module Main where
import Test.Hspec
import Conduit
import Control.Lens
import Data.Monoid
import qualified Data.Conduit.Combinators as DC
import qualified Data.Conduit.List as CL
import Fitparse
import Fitparse.Model
import Fitparse.TH.ProfileMessages

import THSpec
  
main :: IO ()
main = hspec $ do
  -- spec
  describe "Fitparse" $ do
    it "asd" $ do
      --1 `shouldBe` 1
      --return ()
      let sample =  (sourceFile "test/test_2.fit".| readRawFitFile .| takeC 50)
      ls <-runResourceT $  runConduit (sample .| sinkList)
      mapM_ print ls
      


