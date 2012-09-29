module Language.Phonetic.Internal where

import           Data.ByteString (ByteString)

import           Data.Set (Set)

newtype Code enc     = Code {getCode :: ByteString}       deriving (Eq, Show)
newtype Alphabet enc = Alphabet {getAlphabet :: Set Char} deriving (Eq, Show)
