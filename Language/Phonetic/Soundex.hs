{-# LANGUAGE EmptyDataDecls #-}
module Language.Phonetic.Soundex (Soundex) where

import           Data.Char (toUpper)
import           Data.Word (Word8)

import           Data.Array (Array)
import qualified Data.Array as Array
import qualified Data.ByteString.Char8 as ByteString
import qualified Data.Set as Set

import qualified Data.ListLike as ListLike

import           Language.Phonetic.Encoder
import           Language.Phonetic.Internal


table :: Array Char (Either Bool Word8)
table = Array.array ('A', 'Z')
        [ ('A', Left  True ), ('B', Right 1    ), ('C', Right 2    ),
          ('D', Right 3    ), ('E', Left  True ), ('F', Right 1    ),
          ('G', Right 2    ), ('H', Left  False), ('I', Left  True ),
          ('J', Right 2    ), ('K', Right 2    ), ('L', Right 4    ),
          ('M', Right 5    ), ('N', Right 5    ), ('O', Left  True ),
          ('P', Right 1    ), ('Q', Right 2    ), ('R', Right 6    ),
          ('S', Right 2    ), ('T', Right 3    ), ('U', Left  True ),
          ('V', Right 1    ), ('W', Left  False), ('X', Right 2    ),
          ('Y', Left  True ), ('Z', Right 2    ) ]

-- | See <https://en.wikipedia.org/wiki/Soundex> for more info.
data Soundex

instance Encoder Soundex where
    alphabet = Alphabet $ Set.fromList (letters ++ map toUpper letters)
      where letters = "abcdefghijklmnopqrstuvwxyz"

    encodeUnsafe ll =
        case map toUpper (ListLike.toList ll) of
            []      -> error "Language.Phonetic.Soundex.encode"
            (c : s) -> Code . ByteString.pack . (c :) . pad . map toDigit .
                       (go1 (table Array.! c)) . map (table Array.!) $ s
      where
        pad (b1 : b2 : b3 : _) = [b1, b2, b3]
        pad bs                 = bs ++ replicate (3 - length bs) '0'

        go1 (Left _)  s = go2 s
        go1 (Right n) s = collapse n s

        go2 []            = []
        go2 (Right n : s) = n : collapse n s
        go2 (Left _  : s) = go2 s

        collapse _ []               = []
        collapse n (Right n' : l)   | n == n'   = collapse n l
                                    | otherwise = go2 (Right n' : l)
        collapse _ (Left True  : l) = go2 l
        collapse n (Left False : l) = collapse n l

        toDigit = head . (show :: Word8 -> String)
