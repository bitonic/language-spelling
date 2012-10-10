{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Language.Distance.Search.Generic where

import           Data.ListLike (ListLike)
import qualified Data.ListLike as ListLike

import           Data.TSTSet (TSTSet)
import qualified Data.TSTSet as TSTSet
import           Language.Distance
import           Language.Distance.Search.BK (BKTree)
import qualified Language.Distance.Search.BK as BKSearch
import qualified Language.Distance.Search.TST as TSTSearch

-- It would be nice to have the ListLike and EditDistance constraints on the
-- class, but I can't, see <http://hackage.haskell.org/trac/ghc/ticket/7100>.
class Search container full algo | container -> full, container -> algo where
    empty  :: container
    insert :: full -> container -> container
    query  :: Int -> full -> container -> [(full, Distance algo)]

    singleton :: full -> container
    singleton str = insert str empty

    member :: full -> container -> Bool
    member x = not . null . query 0 x

    fromList :: [full] -> container
    fromList = foldr insert empty

-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ --
-- ~~ TST Search ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ --
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ --

newtype TSTDist full sym algo = TSTDist {getTST :: TSTSet sym}

instance (Ord sym, ListLike full sym, EditDistance sym Levenshtein)
         => Search (TSTDist full sym Levenshtein) full Levenshtein where
    empty        = TSTDist TSTSearch.empty
    insert ll    = TSTDist . TSTSearch.insert ll . getTST
    query maxd s = TSTSearch.levenshtein maxd s . getTST

instance (Ord sym, ListLike full sym, EditDistance sym DamerauLevenshtein)
         => Search (TSTDist full sym DamerauLevenshtein) full DamerauLevenshtein where
    empty     = TSTDist TSTSearch.empty
    insert ll = TSTDist . TSTSet.insert (ListLike.toList ll) . getTST
    query maxd s = TSTSearch.damerauLevenshtein maxd s . getTST

-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ --
-- ~~ BK Search ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ --
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ --

newtype BKDist full sym algo = BKDist {getBK :: BKTree full algo}

instance (Eq sym, ListLike full sym, EditDistance sym algo)
         => Search (BKDist full sym algo) full algo where
    empty        = BKDist BKSearch.empty
    insert s     = BKDist . BKSearch.insert s . getBK
    query maxd s = BKSearch.query maxd s . getBK
