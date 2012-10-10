{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Language.Distance.Search.TST
    ( levenshtein
    , damerauLevenshtein
    , TSTSet.empty
    , insert
    , deletions
    , transpositions
    , replaces
    , insertions
    ) where

import           Control.Arrow (first)
import           Data.Word (Word8)

import           Data.ByteString (ByteString)

import           Data.ListLike (ListLike)
import qualified Data.ListLike as ListLike

import           Data.TST (WildCard (..), WildList)
import qualified Data.TST as TST
import           Data.TSTSet (TSTSet)
import qualified Data.TSTSet as TSTSet
import           Language.Distance (Levenshtein, DamerauLevenshtein)
import           Language.Distance.Internal

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
-- ~~ Search ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ --
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ --

levenshtein :: (Ord sym, ListLike full sym) => Int -> full -> TSTSet sym
            -> [(full, Distance Levenshtein)]
levenshtein = queryTST (\s -> deletions s ++ replaces s ++ insertions s)

damerauLevenshtein :: (Ord sym, ListLike full sym) => Int -> full -> TSTSet sym
                   -> [(full, Distance DamerauLevenshtein)]
damerauLevenshtein =
    queryTST (\s -> deletions s ++ replaces s ++ insertions s ++ transpositions s)

insert :: (Ord sym, ListLike full sym) => full -> TSTSet sym -> TSTSet sym
insert ll = TSTSet.insert (ListLike.toList ll)
{-# SPECIALISE insert :: String -> TSTSet Char -> TSTSet Char #-}
{-# SPECIALISE insert :: ByteString -> TSTSet Word8 -> TSTSet Word8 #-}

queryTST :: (Ord sym, ListLike full sym)
         => (WildList sym -> [WildList sym])
         -> Int -> full -> TSTSet sym -> [(full, Distance algo)]
queryTST f maxd s tst =
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
{-# SPECIALISE queryTST :: (WildList Char -> [WildList Char])
                        -> Int -> String -> TSTSet Char
                        -> [(String, Distance algo)] #-}
{-# SPECIALISE queryTST :: (WildList Word8 -> [WildList Word8])
                        -> Int -> ByteString -> TSTSet Word8
                        -> [(ByteString, Distance algo)] #-}

wildList :: ListLike full sym => full -> WildList sym
wildList = map El . ListLike.toList
