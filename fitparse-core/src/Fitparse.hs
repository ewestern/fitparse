{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE LambdaCase #-}
module Fitparse (
    readRawFitFile
  , readFitMessages
  , dataMessageFilter
  , RecordStream
  , module Fitparse.TH.ProfileMessages
  , module Fitparse.TH.ProfileTypes
  )where

import qualified Data.Map as M
import qualified Data.Vector as V
import Conduit
import Control.Lens
import qualified Data.Conduit.List as CL
import           Control.Exception.Base
import qualified Data.ByteString as BS
import Data.Serialize.Get
import Data.Conduit.Cereal hiding (GetException)
import Data.Typeable
import Data.Word
import Data.ByteString.Lazy (ByteString)

import Fitparse.TH.ProfileMessages
import Fitparse.TH.ProfileTypes
import Fitparse.Model
import Fitparse.Parse
import Fitparse.FitMessage



type RecordStream a = ConduitT DataMessageContents a M ()

readFitMessages :: ConduitT BS.ByteString DataMessageContents M ()
readFitMessages = readRawFitFile .| CL.mapMaybe (preview $ _Right . _DataMessage) .| CL.mapMaybe (preview _2)

readRawFitFile :: ConduitT BS.ByteString (Either String Message) M ()
readRawFitFile = do
  gh <- sinkGet getGlobalHeader
  yield $ Right $ GlobalHeaderMessage gh
  messagesConduit

dataMessageFilter :: ConduitT (Either String Message) DataMessageContents  M ()
dataMessageFilter = loop
  where 
    loop = do
      msg <- await
      case msg of 
        (Just (Right (DataMessage _ dmc))) -> do
          yield dmc
          loop
        (Just _) -> loop
        Nothing -> return ()



newtype GetException = GetException String
  deriving (Show, Typeable)

instance Exception GetException


type M = ResourceT IO


type State = M.Map Int (MessageHeader, DefinitionMessageContent)

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
        let newState = maybe state (flip (uncurry M.insert) state) $ getHeaderForMap x
        if BS.null rest
            then awaitNE >>= start newState
            else start newState rest
