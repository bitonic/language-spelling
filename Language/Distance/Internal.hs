{-# LANGUAGE GeneralizedNewtypeDeriving #-}
-- | This module lets you tamper with 'Distance' - in other words you can give
--   it whatever phantom type since the constructor is exported.
--
--   The only use for this module is to construct new 'Distance' data when
--   writing 'EditDistance' instances, but be careful not to change the phantom
--   type of existing 'Distance's!
module Language.Distance.Internal (Distance (..)) where

newtype Distance algo = Distance {getDistance :: Int}
    deriving (Eq, Ord, Show)
