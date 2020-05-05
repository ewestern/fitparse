{-# LANGUAGE QuasiQuotes #-}
module TH.ProfileTypes where

import Data.Serialize.Get
import TH.Types
import FitType


[fittype|profile/types.csv|]
