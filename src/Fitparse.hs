{-# LANGUAGE RankNTypes #-}
module Fitparse where

import qualified Data.Map as M
import Conduit
import           Control.Exception.Base
import qualified Data.ByteString as BS
import Data.Serialize.Get
import Data.Conduit.Cereal hiding (GetException)
import Data.Conduit.Combinators
import Data.Typeable

import Model
import Parse


readFitFile :: ConduitT BS.ByteString (Either String Message) M ()
readFitFile = do
  gh <- sinkGet getGlobalHeader
  yield $ Right $ GlobalHeaderMessage gh
  messagesConduit

  
-- fuseUpstream () messagesConduit
  
  
newtype GetException = GetException String
  deriving (Show, Typeable)

instance Exception GetException

type State = M.Map Int DefinitionMessageContent

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
    result s (Fail msg _) = throwM (GetException (msg <> show s))
    result state (Partial f) = awaitNE >>= (result state) . f
    result state (Done x rest) = do
        yield x
        let newState = maybe state (flip (uncurry M.insert) state) $ getDefMessageContent x
        if BS.null rest
            then awaitNE >>= start newState
            else start newState rest
