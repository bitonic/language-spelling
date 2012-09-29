{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Language.Distance.Search.Class (Search (..)) where

import           Data.ListLike (ListLike)

import           Language.Distance


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

-- class Search container sym algo where
--     empty  :: (EditDistance algo sym, ListLike full sym) => container full sym algo
--     insert :: (EditDistance algo sym, ListLike full sym)
--            => full -> container full sym algo -> container full sym falgo
--     query  :: (EditDistance algo sym, ListLike full sym)
--            => Int -> full -> container full sym algo -> [(full, Distance algo)]

--     singleton :: (EditDistance algo sym, ListLike full sym)
--               => full -> container full sym algo
--     singleton str = insert str empty

--     member :: (EditDistance algo sym, ListLike full sym)
--            => full -> container full sym algo -> Bool
--     member x = not . null . query 0 x

--     fromList :: (EditDistance algo sym, ListLike full sym)
--              => [full] -> container full sym algo
--     fromList = foldr insert empty

