module FitMessage where

import Data.Serialize.Get
import Data.Vector (Vector)
import qualified Data.Vector as V


{-
 Definitions create a 1:1 mapping between localMessageType (0-n) and a global message type: ie. an established Fit Message type
 -}


class Monoid a => FitMessage a where
  messageParserByFieldNumber :: Int -> Get a

  messageParserByFieldNumbers:: Vector Int -> Get a
  messageParserByFieldNumbers vec = do
    v <- traverse messageParserByFieldNumber vec
    return $ foldr (<>) mempty v

