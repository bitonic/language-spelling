{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- | An implementation of 'Language.Distance.Search' based on a BK-tree:
--   <https://en.wikipedia.org/wiki/Bk-tree>.  It performs reasonably, and it
--   scales decently as the query distance increases.  Moreover the data
--   structure can work on any instance of 'EditDistance', or in fact any metric
--   space (although no interface for that purpose is defined):
--   <https://en.wikipedia.org/wiki/Metric_space>.
--
--   However, for very short distances (less than 3),
--   'Language.Distance.Search.TST' is faster.
module Language.Distance.Search.BK
    ( -- * Data type
      BKTree
      -- * Operations
    , empty
    , insert
    , query
    , levenshtein
    , damerauLevenshtein
    ) where

import           Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap

import           Data.ListLike (ListLike)

import           Language.Distance (EditDistance (..), Levenshtein, DamerauLevenshtein)
import           Language.Distance.Internal

data BKTree full algo
    = EmptyBK
    | BKTree !full !(IntMap (BKTree full algo))

narrow :: Int -> Int -> IntMap a -> IntMap a
narrow n m im | n == m    = IntMap.fromList (maybe [] (\v -> [(n, v)]) (IntMap.lookup n im))
narrow n m im | otherwise = insMaybe m res pr
  where
    (_, pl, res0)  = IntMap.splitLookup n im
    (res, pr, _)   = IntMap.splitLookup m (insMaybe n res0 pl)
    insMaybe k im' = maybe im' (\v -> IntMap.insert k v im')

empty :: BKTree full algo
empty = EmptyBK

insert :: forall full sym algo. (Eq sym, EditDistance sym algo, ListLike full sym)
       => full -> BKTree full algo -> BKTree full algo
insert str EmptyBK = BKTree str IntMap.empty
insert str bk@(BKTree str' bks)
    | dist == 0 = bk
    | otherwise = BKTree str' $ flip (IntMap.insert dist) bks $
                  maybe (insert str EmptyBK) (insert str) (IntMap.lookup dist bks)
  where dist = getDistance (distance str str' :: Distance algo)

query :: forall full sym algo. (ListLike full sym, EditDistance sym algo)
      => Int -> full -> BKTree full algo -> [(full, Distance algo)]
query _    _    EmptyBK          = []
query maxd str (BKTree str' bks) = match ++ concatMap (query maxd str) children
  where
    dist     = distance str str' :: Distance algo
    intDist  = getDistance dist
    match    = if (intDist <= maxd) then [(str', dist)] else []
    children = IntMap.elems $ narrow (abs (intDist - maxd)) (intDist + maxd) bks

levenshtein :: (ListLike full sym, EditDistance sym Levenshtein)
            => Int -> full -> BKTree full Levenshtein -> [(full, Distance Levenshtein)]
levenshtein = query

damerauLevenshtein :: (ListLike full sym, EditDistance sym DamerauLevenshtein)
                   => Int -> full -> BKTree full DamerauLevenshtein
                   -> [(full, Distance DamerauLevenshtein)]
damerauLevenshtein = query
