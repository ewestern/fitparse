{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Fitparse.Query 
  (
    Selectable(..)
  , Selecting
  , from
  , where_
  , Only(..)
  , Agg
  , (===)
  , lte
  , gt
  , toList
  , getMessageStream
  , getDataMessages
  )where

import Control.Lens hiding (from)
import Control.Monad (join)
import Control.Monad.Reader
import Conduit
import qualified Data.Conduit.List as CL
import qualified Data.Conduit.Combinators as CC
import qualified Data.Map as M
import Data.Int
import Data.Monoid
import qualified Data.ByteString as B
import Control.Applicative (liftA2)

import Fitparse

import Fitparse.Model
import Fitparse.TH.ProfileMessages

type M = ResourceT IO

type F x = Const (First x)

type OutputStream a = ConduitT () a (ResourceT IO) ()

type Selecting f a =  (Maybe a -> f (Maybe a ) ) 
                   -> DataMessageContents 
                   -> f DataMessageContents


type SelectingRow a = Selecting (F (Maybe a)) a

type Grouping f a t =  (Maybe a -> f (Maybe a ) ) 
                   -> t
                   -> f t

-- Goal a
-- update Selectable class such that
--  it allows for aggregate columns
--  it's type allows for "sink"ing iff All or none of the selected columns are aggregate

---class SelectableColumn from to where
---  selectColumn :: Selecting (F (Maybe to)) to -> from -> Maybe to
---  
---instance SelectableColumn DataMessageContents a where
---  selectColumn selecting = preview' selecting 
---
---instance SelectableColumn DataMessageContents (Agg DataMessageContents a) where
---  selectColumn selecting = preview' selecting

-- semantics: so long as there is a group by statement OR so long as there is an agg function
-- , all items in select must be iether agg or group by
-- 
-- All things that can be grouped can be selected, 
-- but not all things that can be selected can be grouped

avg :: Fractional a 
    => SelectingRow a 
    -> Agg (Maybe a) (Maybe a)
avg selecting = Agg combine retrieve (0,0) (preview' selecting)
  where
    combine (cnt,sm) (Just v) = (cnt+1, sm+v)
    combine tup Nothing = tup
    retrieve (cnt,sm) = Just $ sm / cnt

sum :: Num a
    => SelectingRow a 
    -> Agg (Maybe a) (Maybe a)
sum selecting = Agg (liftA2 (+)) id (Just 0) (preview' selecting)
  

data Agg from to = forall inter. Agg 
  { combine :: inter -> from -> inter
  , retrieve :: inter -> to
  , init :: inter
  , selecting :: DataMessageContents -> from
  }

newtype Only a = Only 
  { unOnly :: a }

class SelectableColumn a where
  type Column a
  selectColumn :: a -> DataMessageContents -> Column a

instance (f ~ F (Maybe a)) => SelectableColumn (Selecting f a)  where
  type Column (Selecting f a) = Maybe a
  selectColumn sel = preview' sel

class Selectable a where
  type Row a
  type Aggregation a
  select :: a -> ConduitT DataMessageContents (Row a) M (Aggregation a)

instance SelectableColumn a => Selectable (Only a) where
  type Row (Only a)  = Column a
  type Aggregation (Only a) = ()
  select (Only sel) = mapC (selectColumn sel)

--instance (f ~ F (Maybe a)) => Selectable (Selecting f a) where
--  type Row  (Selecting f a) = Maybe a
--  type Aggregation (Selecting f a) = ()
--  select sel = mapC (preview' sel)

instance Selectable (Agg from to) where
  type Row (Agg from to) = Maybe from
  type Aggregation (Agg from to) =  to
  select (Agg comb ret init sel) =  fmap ret $ mapC sel .| (CC.foldl comb init)

instance (SelectableColumn a, SelectableColumn b) => Selectable (a, b) where
  type Row (a, b) = (Column a, Column b)
  type Aggregation (a, b) = ()
  select (sa, sb)  = 
    let h              = (selectColumn sa, selectColumn sb)
        i (gsa, gsb) r = (gsa r, gsb r)
    in mapC (i h)

instance (SelectableColumn a, SelectableColumn b, SelectableColumn c) => Selectable (a, b, c) where
  type Row (a, b, c) = (Column a, Column b, Column c)
  type Aggregation (a, b, c) = ()
  select (sa, sb, sc)  = 
    let h              = (selectColumn sa, selectColumn sb, selectColumn sc)
        i (gsa, gsb, gsc) r = (gsa r, gsb r, gsc r)
    in mapC (i h)

    
where_ :: OutputStream a
      -> Getter a (Maybe Bool)
      -> OutputStream a

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
     -> OutputStream a
from rec source =  source .| readFitMessages .| rec

--groupBy' :: Groupable b 
--         => OutputStream a 
--         -> b 
--         -> ConduitT () a (ResourceT IO) (M.Map (Row b) a)
--groupBy' stream select = stream .| foldM folder M.empty 
--  where
--    folder map row = 
--      let key = grouping row
--          row = M.lookup key map
--          updated = aggregate row
--       in M.insert key updated map
-- foldM :: Monad m => (a -> b -> m a) -> a -> ConduitT b o m a 

preview' :: MonadReader s m => Getting (First (Maybe a)) s (Maybe a) -> m (Maybe a)
preview' = fmap join . preview

getDataMessages :: ConduitT (Either String Message) (MessageHeader, DataMessageContents) (ResourceT IO ) ()
getDataMessages = CL.mapMaybe (\x -> x^?(_Right . _DataMessage)) 


getMessageStream :: Getting (First c) DataMessageContents c -> ConduitT (Either String Message) c (ResourceT IO) ()
getMessageStream p =  getDataMessages .| CL.mapMaybe (\x -> x^?_2.p) 

toList :: OutputStream a -> IO [a]
toList selectQuery = do
    let s = selectQuery .| sinkList
    runResourceT $  runConduit s
