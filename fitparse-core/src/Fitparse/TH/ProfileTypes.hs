{-# LANGUAGE QuasiQuotes #-}
module Fitparse.TH.ProfileTypes where

import Data.Serialize.Get
import Fitparse.TH.Types
import Fitparse.FitType


[fittype|profile/types.csv|]
