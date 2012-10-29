-- | Implementation of a BK-tree: <https://en.wikipedia.org/wiki/Bk-tree>
module Data.BKTree
    ( -- * Types
      Distance
    , BKTree
      -- * Operations
    , empty
    , insert
    , query
    ) where

import           Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap

type Distance s = s -> s -> Int

data BKTree s =
    BKTree !(BK s) (Distance s)

data BK s
    = EmptyBK
    | BK !s !(IntMap (BK s))

narrow :: Int -> Int -> IntMap a -> IntMap a
narrow n m im | n == m    = IntMap.fromList (maybe [] (\v -> [(n, v)]) (IntMap.lookup n im))
narrow n m im | otherwise = insMaybe m res pr
  where
    (_, pl, res0)  = IntMap.splitLookup n im
    (res, pr, _)   = IntMap.splitLookup m (insMaybe n res0 pl)
    insMaybe k im' = maybe im' (\v -> IntMap.insert k v im')

empty :: Distance s -- ^ The distance function \"d\" must be a metric on \"s\"
                    --   (<https://en.wikipedia.org/wiki/Metric_space#Definition>):
                    --
                    --   * d x y >= 0
                    --
                    --   * d x y == 0 iff x == y
                    --
                    --   * d x y == d y x
                    --
                    --   * d x z <= d x y + d y z
      -> BKTree s
empty = BKTree EmptyBK

insert :: s -> BKTree s -> BKTree s
insert s (BKTree bk f) = BKTree (insert' s f bk) f

insert' :: s -> Distance s -> BK s -> BK s
insert' s _ EmptyBK = BK s IntMap.empty
insert' s f bk@(BK s' bks)
    | dist == 0 = bk
    | otherwise = BK s' $ flip (IntMap.insert dist) bks $
                  maybe (insert' s f EmptyBK) (insert' s f) (IntMap.lookup dist bks)
  where dist = f s s'

query :: Int                    -- ^ The maximum distance to search for.
      -> s -> BKTree s
      -> [(s, Int)]             -- ^ All the words with a distance less than the
                                -- one specified, and their respective
                                -- distances.
query maxd s (BKTree bk f) = query' maxd s f bk

query' :: Int -> s -> Distance s -> BK s -> [(s, Int)]
query' _    _ _  EmptyBK    = []
query' maxd s f (BK s' bks) = match ++ concatMap (query' maxd s f) children
  where
    dist     = f s s'
    match    = if (dist <= maxd) then [(s', dist)] else []
    children = IntMap.elems $ narrow (max (dist - maxd) 0) (dist + maxd) bks
