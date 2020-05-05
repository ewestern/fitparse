{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE LambdaCase #-}
module Fitparse where

import qualified Data.Map as M
import qualified Data.Vector as V
import Conduit
import           Control.Exception.Base
import qualified Data.ByteString as BS
import Data.Serialize.Get
import Data.Conduit.Cereal hiding (GetException)
-- import qualified Data.Conduit.Combinators as (DC)
import Data.Typeable
import Data.Word
import Data.ByteString.Lazy (ByteString)

import TH.ProfileMessages
import Model
import Parse
import FileTypes
import FitMessage

readFitFile :: ConduitT BS.ByteString (Either String Message) M ()
readFitFile = do
  gh <- sinkGet getGlobalHeader
  yield $ Right $ GlobalHeaderMessage gh
  messagesConduit

newtype GetException = GetException String
  deriving (Show, Typeable)

instance Exception GetException

type State = M.Map Int (Get DataMessageContents)

type M = ResourceT IO

messagesConduit :: ConduitT BS.ByteString (Either String Message) M ()
messagesConduit  =
    awaitNE >>= start M.empty
  where
    awaitNE =
        loop
      where
        loop = await >>= maybe (return BS.empty) check
        check bs
            | BS.null bs = loop
            | otherwise = return bs

    start :: State -> BS.ByteString -> ConduitT BS.ByteString (Either String Message) M ()
    start state bs
        | BS.null bs = return ()
        | otherwise = 
          let get = getMessage state
          in result state (runGetPartial get bs)


    result :: State -> Result (Either String Message) -> ConduitT BS.ByteString (Either String Message) M ()
    result s (Fail msg _) = do
      yield $ Left msg
      awaitNE >>= start s
    result state (Partial f) = awaitNE >>= (result state) . f
    result state (Done x rest) = do
        yield x
        let newState = maybe state (flip (uncurry M.insert) state) $ getParserForMessage x
        if BS.null rest
            then awaitNE >>= start newState
            else start newState rest
