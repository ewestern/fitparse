module Fitparse.FitMessage where

import Data.Serialize.Get
import Data.Vector (Vector)
import qualified Data.Vector as V
import Fitparse.BaseTypes


{-
 Definitions create a 1:1 mapping between localMessageType (0-n) and a global message type: ie. an established Fit Message type
 -}


class Monoid a => FitMessage a where
  messageParserByField :: FieldDefinitionContents -> Get a

  messageParserByFields:: Vector FieldDefinitionContents -> Get a
  messageParserByFields vec = do
    v <- traverse messageParserByField vec
    return $ foldr (<>) mempty v

-- Putting this here to resolve circular import, but would ideally live in Fitparse.Model
data FieldDefinitionContents
  = FieldDefinitionContents {
  fieldDefinitionNumber :: Int,
  fieldSize :: Int, -- in bytes
  baseType :: Maybe BaseType
}
  deriving (Eq, Ord, Show)

