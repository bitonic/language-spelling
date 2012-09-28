{-# LANGUAGE MultiParamTypeClasses #-}
module Language.Distance.Search.Class (Search (..)) where

import           Data.ListLike (ListLike)

import           Language.Distance


class Search container sym algo where
    empty  :: (EditDistance algo sym, ListLike full sym) => container full sym algo
    insert :: (EditDistance algo sym, ListLike full sym)
           => full -> container full sym algo -> container full sym algo
    query  :: (EditDistance algo sym, ListLike full sym)
           => Int -> full -> container full sym algo -> [(full, Distance algo)]

    singleton :: (EditDistance algo sym, ListLike full sym)
              => full -> container full sym algo
    singleton str = insert str empty

    member :: (EditDistance algo sym, ListLike full sym)
           => full -> container full sym algo -> Bool
    member x = not . null . query 0 x

