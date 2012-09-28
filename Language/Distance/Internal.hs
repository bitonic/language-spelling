{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Language.Distance.Internal (Distance (..)) where

import           Data.Hashable

newtype Distance algo = Distance {getDistance :: Int}
    deriving (Eq, Ord, Read, Show, Hashable)

