-- | See the documentation for 'Language.Distance.Internal' to see why this
--   module is potentially dangerous.
module Language.Phonetic.Internal where

import           Data.ByteString (ByteString)

import           Data.Set (Set)

-- | A wrapped 'ByteString'.
newtype Code enc     = Code {getCode :: ByteString}       deriving (Eq, Show)
-- | A wrapped 'Set' 'Char'.
newtype Alphabet enc = Alphabet {getAlphabet :: Set Char} deriving (Eq, Show)
