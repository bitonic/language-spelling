{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Language.Distance
    ( Distance
    , getDistance
    , EditDistance (..)
    , Levenshtein
    , DamerauLevenshtein
    ) where

import           Control.Applicative ((<$>))
import           Control.Monad (forM_, foldM_)
import           Control.Monad.ST (runST)
import           Data.Monoid ((<>))

import qualified Data.HashMap.Strict as HashMap
import           Data.Hashable (Hashable)
import           Data.ListLike (ListLike)
import qualified Data.ListLike as LL
import qualified Data.Vector.Unboxed.Mutable as Vector

import Prelude hiding ((!!))


newtype Distance algo = Distance {getDistance :: Int}
    deriving (Eq, Ord, Read, Show)

class EditDistance algo sym where
    distance :: ListLike full sym => full -> full -> Distance algo

-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ --
-- ~~ Levenshtein ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ --
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ --

data Levenshtein

instance Eq sym => EditDistance Levenshtein sym where
    distance s1 s2 = runST $ do v <- Vector.replicate ((len1 + 1) * (len2 + 1)) 0
                                mapM_ (\r -> Vector.write v (ix r 0) r) [0..len1]
                                mapM_ (\c -> Vector.write v (ix 0 c) c) [0..len2]
                                forM_ [(r, c) | r <- [1..len1], c <- [1..len2]] (go v)
                                Distance <$> Vector.read v (Vector.length v - 1)
      where
        len1 = LL.length s1
        len2 = LL.length s2
        ix r c = r * (len2 + 1) + c
        go v (r, c) = do d1 <- (+1) <$> Vector.read v (ix (r - 1) c)
                         d2 <- (+1) <$> Vector.read v (ix r (c - 1))
                         d3 <- (if s1 !! (r - 1) == s2 !! (c - 1) then id else (+1))
                               <$> Vector.read v (ix (r - 1) (c - 1))
                         Vector.write v (ix r c) (min3 d1 d2 d3)

-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ --
-- ~~ Damerau-Levenshtein ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ --
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ --

data DamerauLevenshtein

instance (Eq sym, Hashable sym) => EditDistance DamerauLevenshtein sym where
    distance s1 s2
        | LL.null s1 = Distance (LL.length s2)
        | otherwise  = runST $ do v <- Vector.replicate ((len1 + 2) * (len2 + 2)) 0
                                  let inf = (len1 + len2)
                                  Vector.write v 0 inf
                                  mapM_ (\r -> Vector.write v (ix (r + 1) 0) inf >>
                                               Vector.write v (ix (r + 1) 1) r) [0..len1]
                                  mapM_ (\c -> Vector.write v (ix 0 (c + 1)) inf >>
                                               Vector.write v (ix 1 (c + 1)) c) [0..len2]
                                  foldM_ (go1 v) chPos' [1..len1]
                                  Distance <$> Vector.read v (ix (len1 + 1) (len2 + 1))
      where
        len1 = LL.length s1
        len2 = LL.length s2
        ix r c = r * (len2 + 2) + c
        chPos' = LL.foldr (\ch -> HashMap.insert ch 0) HashMap.empty (s1 <> s2)
        go1 v chPos r = do foldM_ (go2 v chPos r) 0 [1..len2]
                           return (HashMap.insert (s1 !! (r - 1)) r chPos)
        go2 v chPos r db c =
            let ch1 = s1 !! (r - 1)
                ch2 = s2 !! (c - 1)
                r1 = chPos HashMap.! ch2
                c1 = db
            in do d1 <- Vector.read v (ix r c)
                  (d2, db') <- if ch1 == ch2 then return (d1, c)
                               else do d3 <- Vector.read v (ix (r + 1) c)
                                       d4 <- Vector.read v (ix r (c + 1))
                                       return (min3 d1 d3 d4, db)
                  d5 <- Vector.read v (ix (r + 1) (c + 1))
                  d6 <- (\n -> n + (r - r1 - 1) + 1 + (c - c1 - 1)) <$>
                        Vector.read v (ix r1 c1)
                  Vector.write v (ix (r + 1) (c + 1)) (min3 d2 d5 d6)
                  return db'


-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ --
-- ~~ Hamming ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ --
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ --

-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ --
-- ~~ Longest Common Subsequence ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ --
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ --

-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ --
-- ~~ Utils ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ --
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ --

min3 :: Ord a => a -> a -> a -> a
min3 x y z = min x (min y z)

(!!) :: ListLike full sym => full -> Int -> sym
(!!) = LL.index
