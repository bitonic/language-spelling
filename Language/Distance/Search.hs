{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
-- | The 'Search' typeclass lets you build dictinaries and then query them to
--   find words close to a given one.
--
--   Right now two data types are provided: 'TST.TST' and 'BK.BK', monomorphic
--   functions are provided as well.  The difference is in performance:
--   'TST.TST' is faster for low distances (less than 3) but impractical for
--   larger ones, where 'BK.BK' is more suited.  See the specific modules for
--   more info.
module Language.Distance.Search
    ( Search (..)
    , TSTDist (..)
    , BKDist (..)
    ) where

import           Data.ListLike (ListLike)
import qualified Data.ListLike as ListLike

import           Data.TSTSet (TSTSet)
import qualified Data.TSTSet as TSTSet
import           Language.Distance
import           Language.Distance.Search.BK (BKTree)
import qualified Language.Distance.Search.BK as BK
import           Language.Distance.Search.Class
import qualified Language.Distance.Search.TST as TST

-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ --
-- ~~ TST Search ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ --
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ --

-- | We need to wrap 'TSTSet' in a newtype because we need the algorithm and the
--   container have to depend on the type.
newtype TSTDist full sym algo = TSTDist {getTST :: TSTSet sym}

instance (Ord sym, ListLike full sym, EditDistance sym Levenshtein)
         => Search (TSTDist full sym Levenshtein) full Levenshtein where
    empty        = TSTDist TST.empty
    insert ll    = TSTDist . TST.insert ll . getTST
    query maxd s = TST.levenshtein maxd s . getTST

instance (Ord sym, ListLike full sym, EditDistance sym DamerauLevenshtein)
         => Search (TSTDist full sym DamerauLevenshtein) full DamerauLevenshtein where
    empty     = TSTDist TST.empty
    insert ll = TSTDist . TSTSet.insert (ListLike.toList ll) . getTST
    query maxd s = TST.damerauLevenshtein maxd s . getTST

-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ --
-- ~~ BK Search ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ --
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ --

-- | Again, wrapping 'BKTree' to have the phantom types in place.
newtype BKDist full sym algo = BKDist {getBK :: BKTree full algo}

instance (Eq sym, ListLike full sym, EditDistance sym algo)
         => Search (BKDist full sym algo) full algo where
    empty        = BKDist BK.empty
    insert s     = BKDist . BK.insert s . getBK
    query maxd s = BK.query maxd s . getBK
