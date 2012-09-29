{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Language.Phonetic.Encoder (Encoder (..)) where

import qualified Data.Set as Set

import           Data.ListLike (ListLike)
import qualified Data.ListLike as ListLike

import           Language.Phonetic.Internal


class Encoder enc where
    alphabet     :: Alphabet enc
    encodeUnsafe :: ListLike full Char => full -> Code enc

    encode :: ListLike full Char => full -> Maybe (Code enc)
    encode ll | ListLike.all (flip Set.member alph) ll = Just (encodeUnsafe ll)
              | otherwise                              = Nothing
      where alph = getAlphabet (alphabet :: Alphabet enc)
