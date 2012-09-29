{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Language.Distance.Search.TST
    ( TSTDist
    , changeAlgo
    , deletions
    , transpositions
    , replaces
    , insertions
    ) where

import           Control.Arrow (first)
import           Data.Word (Word8)

import           Control.DeepSeq
import           Data.ByteString (ByteString)

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

changeAlgo :: TSTDist full sym algo1 -> TSTDist full sym algo1
changeAlgo (TSTDist tst) = TSTDist tst

instance (Ord sym, ListLike full sym, EditDistance Levenshtein sym)
         => Search (TSTDist full sym Levenshtein) full Levenshtein where
    empty     = TSTDist TSTSet.empty
    insert ll = TSTDist . TSTSet.insert (ListLike.toList ll) . getTST
    {-# SPECIALISE insert :: String -> TSTDist String Char Levenshtein
                          -> TSTDist String Char Levenshtein #-}
    {-# SPECIALISE insert :: ByteString -> TSTDist ByteString Word8 Levenshtein
                          -> TSTDist ByteString Word8 Levenshtein #-}
    query     = queryTST (\s -> deletions s ++ replaces s ++ insertions s)
    {-# SPECIALISE query :: Int -> String -> TSTDist String Char Levenshtein
                         -> [(String, Distance Levenshtein)] #-}
    {-# SPECIALISE query :: Int -> ByteString -> TSTDist ByteString Word8 Levenshtein
                         -> [(ByteString, Distance Levenshtein)] #-}


instance (Ord sym, ListLike full sym, EditDistance DamerauLevenshtein sym)
         => Search (TSTDist full sym DamerauLevenshtein) full DamerauLevenshtein where
    empty     = TSTDist TSTSet.empty
    insert ll = TSTDist . TSTSet.insert (ListLike.toList ll) . getTST
    {-# SPECIALISE insert :: String -> TSTDist String Char DamerauLevenshtein
                          -> TSTDist String Char DamerauLevenshtein #-}
    {-# SPECIALISE insert :: ByteString -> TSTDist ByteString Word8 DamerauLevenshtein
                          -> TSTDist ByteString Word8 DamerauLevenshtein #-}
    query     = queryTST (\s -> deletions s ++ replaces s ++ insertions s ++
                                transpositions s)
    {-# SPECIALISE query :: Int -> String -> TSTDist String Char DamerauLevenshtein
                         -> [(String, Distance DamerauLevenshtein)] #-}
    {-# SPECIALISE query :: Int -> ByteString -> TSTDist ByteString Word8 DamerauLevenshtein
                         -> [(ByteString, Distance DamerauLevenshtein)] #-}

queryTST :: (Ord sym, ListLike full sym)
         => (WildList sym -> [WildList sym])
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
{-# SPECIALISE queryTST :: (WildList Char -> [WildList Char])
                        -> Int -> String -> TSTDist String Char Levenshtein
                        -> [(String, Distance Levenshtein)] #-}
{-# SPECIALISE queryTST :: (WildList Char -> [WildList Char])
                        -> Int -> String -> TSTDist String Char DamerauLevenshtein
                        -> [(String, Distance DamerauLevenshtein)] #-}
{-# SPECIALISE queryTST :: (WildList Word8 -> [WildList Word8])
                        -> Int -> ByteString -> TSTDist ByteString Word8 Levenshtein
                        -> [(ByteString, Distance Levenshtein)] #-}
{-# SPECIALISE queryTST :: (WildList Word8 -> [WildList Word8])
                        -> Int -> ByteString -> TSTDist ByteString Word8 DamerauLevenshtein
                        -> [(ByteString, Distance DamerauLevenshtein)] #-}

wildList :: ListLike full sym => full -> WildList sym
wildList = map El . ListLike.toList
