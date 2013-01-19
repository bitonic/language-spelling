{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Language.Distance.Search.Class (Search (..)) where

import           Language.Distance

-- | Generic class for data structures that can perform queries retrieving words
--   close to a given one.
--
--   Minimal definition: 'empty', 'insert', and 'query'.

-- It would be nice to have the ListLike and EditDistance constraints on the
-- class, but I can't, see <http://hackage.haskell.org/trac/ghc/ticket/7100>.
class Search container full algo | container -> full, container -> algo where
    empty  :: container
    insert :: full -> container -> container
    query  :: Int               -- ^ The maximum distance to search for
           -> full              -- ^ The starting word
           -> container
           -> [(full, Distance algo)]

    singleton :: full -> container
    singleton str = insert str empty

    member :: full -> container -> Bool
    member x = not . null . query 0 x

    fromList :: [full] -> container
    fromList = foldr insert empty
