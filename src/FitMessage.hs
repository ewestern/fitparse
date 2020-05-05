module FitMessage where

import Data.Serialize.Get


{-
 Definitions create a 1:1 mapping between localMessageType (0-n) and a global message type: ie. an established Fit Message type
 -}


class FitMessage a where
  messageParser :: Get a
