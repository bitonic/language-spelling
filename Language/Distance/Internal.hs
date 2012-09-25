module Language.Distance.Internal (Distance (..))where

newtype Distance algo = Distance {getDistance :: Int}
    deriving (Eq, Ord, Read, Show)
