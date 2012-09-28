{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Language.Distance.Search.TST
    ( TSTDist
    , deletions
    , transpositions
    , replaces
    , insertions
    ) where

import           Control.Arrow (first)

import           Control.DeepSeq

import           Data.ListLike (ListLike)
import qualified Data.ListLike as ListLike

import           Data.TST (WildCard (..), WildList)
import qualified Data.TST as TST
import           Data.TSTSet (TSTSet)
import qualified Data.TSTSet as TSTSet
import           Language.Distance
import           Language.Distance.Internal
import           Language.Distance.Search.Class (Search)
import qualified Language.Distance.Search.Class as Class

-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ --
-- ~~ Edits ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ --
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ --

deletions, transpositions :: [a] -> [[a]]
replaces, insertions :: WildList a -> [WildList a]

deletions []      = []
deletions (c : s) = s : map (c :) (deletions s)

transpositions []  = []
transpositions [x] = [[x]]
transpositions (x : y : s) = (y : x : s) : map (x :) (transpositions (y : s))

replaces [] = []
replaces (c : s) = (WildCard : s) : map (c :) (replaces s)

insertions [] = [[WildCard]]
insertions (c : s) = (WildCard : c : s) : map (c :) (insertions s)

-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ --
-- ~~ TST Search ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ --
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ --

newtype TSTDist full sym algo = TSTDist {getTST :: TSTSet sym}

instance NFData sym => NFData (TSTDist full sym algo) where
    rnf = rnf . getTST

instance (Ord sym, Show sym) => Search TSTDist sym Levenshtein where
    empty     = TSTDist TSTSet.empty
    insert ll = TSTDist . TSTSet.insert (ListLike.toList ll) . getTST
    query     = queryTST (\s -> deletions s ++ replaces s ++ insertions s)

instance (Ord sym, Show sym) => Search TSTDist sym DamerauLevenshtein where
    empty     = TSTDist TSTSet.empty
    insert ll = TSTDist . TSTSet.insert (ListLike.toList ll) . getTST
    query     = queryTST (\s -> deletions s ++ replaces s ++ insertions s ++
                                transpositions s)


queryTST :: (Ord sym, ListLike full sym, Show sym)
         => ([WildCard sym] -> [[WildCard sym]])
         -> Int -> full -> TSTDist full sym algo -> [(full, Distance algo)]
queryTST f maxd s (TSTDist tst) =
    map (first ListLike.fromList) $ TST.toList $
    go 0 TSTSet.empty [wildList $ ListLike.toList s] TST.empty
  where
    go n visited edits matches
        | n <= maxd = let edits'     = filter (\x -> not (TSTSet.member x visited)) edits
                          newMatches = zip (concatMap (flip TSTSet.matchWL tst) edits')
                                           (map Distance (repeat n))
                      in go (n + 1) (foldr TSTSet.insert visited edits')
                            (concatMap f edits') (update newMatches matches)
        | otherwise = matches

    update new matches = foldr (uncurry (TST.insertWith (flip const))) matches new

wildList :: ListLike full sym => full -> WildList sym
wildList = map El . ListLike.toList
