{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Fitparse.Query where

import Control.Lens
import Control.Monad (join)
import Control.Monad.Reader
import Conduit
import qualified Data.Conduit.List as CL
import Data.Int
import Data.Monoid
import qualified Data.ByteString as B
import Control.Applicative (liftA2)

import Fitparse
import Fitparse.Model
import Fitparse.TH.ProfileMessages

type M = ResourceT IO

type F x = Const (First x)

type Selecting f a =  (Maybe a -> f (Maybe a ) ) -> DataMessageContents -> f DataMessageContents

--class Aggregatable a where
--  foo :: 

class Selectable a where
  type Row a
  select :: a -> RecordStream (Row a)

instance (f ~ F (Maybe a))
  => Selectable (Selecting f a) where
  type Row  (Selecting f a) = Maybe a
  select sel = mapC (preview' $ sel)

instance (f ~ F (Maybe a), g ~ F (Maybe b))
  => Selectable (Selecting f a, Selecting g b) where
  type Row  (Selecting f a, Selecting g b) = (Maybe a, Maybe b)
  select (sa, sb)  = 
    let h              = (preview' sa, preview' sb)
        i (gsa, gsb) r = (gsa r, gsb r)
    in mapC (i h)

instance (f ~ F (Maybe a), g ~ F (Maybe b), h ~ F (Maybe c))
  => Selectable (Selecting f a, Selecting g b, Selecting h c) where
  type Row  (Selecting f a, Selecting g b, Selecting h c) = (Maybe a, Maybe b, Maybe c)
  select (sa, sb, sc)  = 
    let h              = (preview' sa, preview' sb, preview' sc)
        i (gsa, gsb, gsc) r = (gsa r, gsb r, gsc r)
    in mapC (i h)
 
instance (f ~ F (Maybe a), g ~ F (Maybe b), h ~ F (Maybe c), i ~ F (Maybe d))
  => Selectable (Selecting f a, Selecting g b, Selecting h c, Selecting i d) where
  type Row  (Selecting f a, Selecting g b, Selecting h c, Selecting i d) = (Maybe a, Maybe b, Maybe c, Maybe d)
  select (sa, sb, sc, sd)  = 
    let h              = (preview' sa, preview' sb, preview' sc, preview' sd)
        i (gsa, gsb, gsc, gsd) r = (gsa r, gsb r, gsc r, gsd r)
    in mapC (i h)
 
    

where_ :: ConduitT raw a (ResourceT IO) ()
      -> Getter a (Maybe Bool)
      -> ConduitT raw a (ResourceT IO) ()
where_ con sb = con .| filterC (any id . preview' sb)



and :: Getter a (Maybe Bool) -> Getter a (Maybe Bool) -> Getter a (Maybe Bool)
and sa sb = runGetter ((liftA2 (&&)) <$> Getter sa <*> Getter sb)

not :: Getter a (Maybe Bool)
      -> Getter a (Maybe Bool)
not sa = sa.(to $ fmap Prelude.not) 

mkPred :: (a -> a -> Bool)
      -> Getter b (Maybe a)
      -> a 
      -> Getter b (Maybe Bool)
mkPred f sa v = sa.pred
  where pred = to $ fmap (flip f v)

(===) :: Eq a
      => Getter m (Maybe a)
      -> a 
      -> Getter m (Maybe Bool)
(===) = mkPred (==)

lte :: Ord a
      => Getter b (Maybe a)
      -> a 
      -> Getter b (Maybe Bool)
lte = mkPred (<=)

gt :: Ord a 
      => Getter b (Maybe a)
      -> a 
      -> Getter b (Maybe Bool)
gt = mkPred (>)



from :: RecordStream a 
     -> ConduitT () B.ByteString (ResourceT IO) () 
     -> ConduitT () a (ResourceT IO) ()
from rec source =  source .| readFitMessages .| rec


preview' :: MonadReader s m => Getting (Data.Monoid.First (Maybe a)) s (Maybe a) -> m (Maybe a)
preview' = fmap join . preview

getDataMessages :: ConduitT (Either String Message) (MessageHeader, DataMessageContents) (ResourceT IO ) ()
getDataMessages = CL.mapMaybe (\x -> x^?(_Right . _DataMessage)) 


getMessageStream :: Getting (First c) DataMessageContents c -> ConduitT (Either String Message) c (ResourceT IO) ()
getMessageStream p =  getDataMessages .| CL.mapMaybe (\x -> x^?_2.p) 


-- groupBy :: ConduitT () a (ResourceT IO) ()
--         -> Selectable a
--         -> 
