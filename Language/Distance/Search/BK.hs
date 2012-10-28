{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- | An implementation of 'Language.Distance.Search' based on a BK-tree:
--   <https://en.wikipedia.org/wiki/Bk-tree>.  It performs reasonably, and it
--   scales decently as the query distance increases.  Moreover the data
--   structure can work on any instance of 'EditDistance', or in fact any metric
--   space - a generic interface is provided in 'Data.BKTree'.
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

import           Control.Arrow (second)

import           Data.ListLike (ListLike)

import qualified Data.BKTree as BKTree
import           Language.Distance (EditDistance (..), Levenshtein, DamerauLevenshtein)
import           Language.Distance.Internal

newtype BKTree full algo = BKTree (BKTree.BKTree full)

empty :: forall full sym algo. (EditDistance sym algo, ListLike full sym)
      => BKTree full algo
empty = BKTree (BKTree.empty (\s s' -> getDistance (distance s s' :: Distance algo)))

insert :: (EditDistance sym algo, ListLike full sym)
       => full -> BKTree full algo -> BKTree full algo
insert s (BKTree bk) = BKTree (BKTree.insert s bk)

query :: (ListLike full sym, EditDistance sym algo)
      => Int -> full -> BKTree full algo -> [(full, Distance algo)]
query maxd s (BKTree bk) = map (second Distance) $ BKTree.query maxd s bk

levenshtein :: (ListLike full sym, EditDistance sym Levenshtein)
            => Int -> full -> BKTree full Levenshtein -> [(full, Distance Levenshtein)]
levenshtein = query

damerauLevenshtein :: (ListLike full sym, EditDistance sym DamerauLevenshtein)
                   => Int -> full -> BKTree full DamerauLevenshtein
                   -> [(full, Distance DamerauLevenshtein)]
damerauLevenshtein = query
