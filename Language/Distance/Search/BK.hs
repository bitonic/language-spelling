{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Language.Distance.Search.BK
    ( BKTree
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

data BKTree full algo = EmptyBK
                      | BKTree !full !(IntMap (BKTree full algo))

narrow :: Int -> Int -> IntMap a -> IntMap a
narrow n m im = fst (IntMap.split m (snd (IntMap.split n im)))

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
