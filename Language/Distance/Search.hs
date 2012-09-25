{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Language.Search
    ( Search (..)
    , BKTree
    ) where

import           Data.IntMap.Lazy (IntMap)
import qualified Data.IntMap.Lazy as IntMap

import           Data.ListLike (ListLike)

import Language.Distance

class Search container sym where
    empty  :: (EditDistance algo sym, ListLike full sym) => container full sym algo
    insert :: (EditDistance algo sym, ListLike full sym)
           => full -> container full sym algo -> container full sym algo
    query  :: (EditDistance algo sym, ListLike full sym)
           => Int -> full -> container full sym algo -> [(Distance algo, full)]

    singleton :: (EditDistance algo sym, ListLike full sym)
              => full -> container full sym algo
    singleton str = insert str empty


-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ --
-- ~~ BK Tree ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ --
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ --

data BKTree full sym algo = EmptyBK | BKTree full (IntMap (BKTree full sym algo))

narrow :: IntMap.Key -> IntMap.Key -> IntMap a -> IntMap a
narrow n m im = fst (IntMap.split m (snd (IntMap.split n im)))

instance Eq sym => Search BKTree sym where
    empty = EmptyBK
    insert = insertBK

    query _    _    EmptyBK          = []
    query maxd str (BKTree str' bks) = match ++ concatMap (query maxd str) children
      where dist = distance str str'
            intDist = getDistance dist
            match | intDist <= maxd = [(dist, str')]
                  | otherwise       = []
            children = IntMap.elems $ narrow (abs (intDist - maxd)) (intDist + maxd) bks

insertBK :: forall full sym algo. (Eq sym, EditDistance algo sym, ListLike full sym)
         => full -> BKTree full sym algo -> BKTree full sym algo
insertBK str EmptyBK = BKTree str IntMap.empty
insertBK str bk@(BKTree str' bks)
    | dist == 0 = bk
    | otherwise = BKTree str' $ flip (IntMap.insert dist) bks $
                  maybe (singleton str) (insertBK str) (IntMap.lookup dist bks)
  where dist = getDistance (distance str str' :: Distance algo)
