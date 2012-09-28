{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Language.Distance.Search.BK (BKTree) where

import           Control.DeepSeq
import           Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap

import           Data.ListLike (ListLike)

import           Language.Distance
import           Language.Distance.Search.Class

data BKTree full sym algo = EmptyBK
                          | BKTree !full !(IntMap (BKTree full sym algo))

instance NFData full => NFData (BKTree full sym algo) where
    rnf EmptyBK       = ()
    rnf (BKTree s im) = rnf s `seq` rnf im

narrow :: IntMap.Key -> IntMap.Key -> IntMap a -> IntMap a
narrow n m im = fst (IntMap.split m (snd (IntMap.split n im)))

instance Eq sym => Search BKTree sym algo where
    empty = EmptyBK
    insert = insertBK
    {-# SPECIALISE insert :: String -> BKTree String Char Levenshtein
                          -> BKTree String Char Levenshtein #-}
    {-# SPECIALISE insert :: String -> BKTree String Char DamerauLevenshtein
                          -> BKTree String Char DamerauLevenshtein #-}

    query _    _    EmptyBK          = []
    query maxd str (BKTree str' bks) = match ++ concatMap (query maxd str) children
      where dist = distance str str'
            intDist = getDistance dist
            match | intDist <= maxd = [(str', dist)]
                  | otherwise       = []
            children = IntMap.elems $ narrow (abs (intDist - maxd)) (intDist + maxd) bks
    {-# SPECIALISE query :: Int -> String -> BKTree String Char Levenshtein
                         -> [(String, Distance Levenshtein)] #-}
    {-# SPECIALISE query :: Int -> String -> BKTree String Char DamerauLevenshtein
                         -> [(String, Distance DamerauLevenshtein)] #-}


insertBK :: forall full sym algo. (Eq sym, EditDistance algo sym, ListLike full sym)
         => full -> BKTree full sym algo -> BKTree full sym algo
insertBK str EmptyBK = BKTree str IntMap.empty
insertBK str bk@(BKTree str' bks)
    | dist == 0 = bk
    | otherwise = BKTree str' $ flip (IntMap.insert dist) bks $
                  maybe (singleton str) (insertBK str) (IntMap.lookup dist bks)
  where dist = getDistance (distance str str' :: Distance algo)
