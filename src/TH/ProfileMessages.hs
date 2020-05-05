{-# LANGUAGE QuasiQuotes #-}
module TH.ProfileMessages where

import Data.Int
import Data.Word
import Data.Map (fromList)

import BaseTypes
import TH.Messages
import TH.ProfileTypes
import FitMessage
import FitType

[fitmsg|profile/messages.csv|]
