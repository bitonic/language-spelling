{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Language.Phonetic.Encoder
    ( Code
    , Encoder (..)
    ) where

import qualified Data.Set as Set

import           Data.ListLike (ListLike)
import qualified Data.ListLike as ListLike

import           Language.Phonetic.Internal


-- | Type class for algorithms that encode words based on their pronunciation.
--
--   Minimal definition: 'alphabet', 'encodeUnsafe'.
class Encoder enc where
    -- | The range of characters that the algorithm will accept
    alphabet     :: Alphabet enc
    -- | Unchecked encoding: if characters not in 'alphabet' are in the word the
    --   behaviour is undefined.  You should use 'encode'.
    encodeUnsafe :: ListLike full Char => full -> Code enc

    -- | Safe 'encodeUnsafe': checks that all the characters are in 'alphabet'
    --   first.
    encode :: ListLike full Char => full -> Maybe (Code enc)
    encode ll | ListLike.all (flip Set.member alph) ll = Just (encodeUnsafe ll)
              | otherwise                              = Nothing
      where alph = getAlphabet (alphabet :: Alphabet enc)
