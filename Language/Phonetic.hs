-- | \"A phonetic algorithm is an algorithm for indexing of words by their
--   pronunciation\": <https://en.wikipedia.org/wiki/Phonetic_algorithm>.
--
--   Right now only a very rudimental one is provided, 'Soundex'; plus
--   'Encoder', a generic interface.
module Language.Phonetic
    ( -- * Types
      Code
    , Alphabet
    , Encoder (..)
      -- * Algorithms
    , Soundex
    ) where

import Language.Phonetic.Encoder
import Language.Phonetic.Soundex
import Language.Phonetic.Internal