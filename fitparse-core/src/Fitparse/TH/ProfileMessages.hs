{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
module Fitparse.TH.ProfileMessages where

import Data.Int
import Data.Word
import Data.Map (fromList)
import Control.Lens

import Fitparse.BaseTypes
import Fitparse.TH.Messages
import Fitparse.TH.ProfileTypes
import Fitparse.FitMessage
import Fitparse.FitType

[fitmsg|profile/messages.csv|]


makePrisms ''DataMessageContents