{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
-- | An implementation of 'Search' based on a ternary search tree
--   ('Data.TSTSet'): <https://en.wikipedia.org/wiki/Ternary_search_tree>.  
--
--   The searches are performed by manually generating the close word by
--   deleting, transposing, or adding wildcards to match additional characters.
--
--   This makes this structure fast for small distances with a small number of
--   generated candidates, but impractical for even slightly larger distances -
--   in my tests 'Language.Distance.Search.BK' outpeforms this module when the
--   distance is greater than 2.
--
--   The data structure has no knowledge of the distance and thus it does not
--   need to be rebuilt if different edit distances are needed.  However this
--   means that it cannot work with arbitrary 'EditDistance' instances are
--   functions need to be defined manually to generate the candidates.  In this
--   case 'levenshtein' uses 'deletions', 'replaces', and 'insertions' to
--   generate the candidates; while 'damerauLevenshtein' also uses
--   'transpositions'.
module Language.Distance.Search.TST
    ( -- * Type
      TSTSet
      -- * Operations
    , TSTSet.empty
    , insert
    , levenshtein
    , damerauLevenshtein
      -- * Candidates generation
      -- $Wildcard
      -- ** Types
    , WildCard (..)
    , WildList
      -- ** Operations
    , deletions
    , transpositions
    , replaces
    , insertions
    ) where

import           Control.Arrow (first)
import           Data.Word (Word8)

import           Data.ByteString (ByteString)
import           Data.Text (Text)

import           Data.ListLike (ListLike)
import qualified Data.ListLike as ListLike
import           Data.ListLike.Text ()

import           Data.TST (WildCard (..), WildList)
import qualified Data.TST as TST
import           Data.TSTSet (TSTSet)
import qualified Data.TSTSet as TSTSet
import           Language.Distance (Levenshtein, DamerauLevenshtein)
import           Language.Distance.Internal

-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ --
-- ~~ Edits ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ --
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ --

{- $Wildcard

The following functions generate candidates distant 1 from the given word, and
they are reexported for completeness.

'deletions' and 'transpositions' do not need to wildcard the word, while
'replaces' and 'insertions' do since we are adding characters.

-}

deletions, transpositions :: [a] -> [[a]]
replaces, insertions :: WildList a -> [WildList a]

deletions []      = []
deletions (c : s) = s : map (c :) (deletions s)

transpositions []  = []
transpositions [x] = [[x]]
transpositions (x : y : s) = (y : x : s) : map (x :) (transpositions (y : s))

replaces [] = []
replaces (c : s) = (WildCard : s) : map (c :) (replaces s)

insertions [] = [[WildCard]]
insertions (c : s) = (WildCard : c : s) : map (c :) (insertions s)

-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ --
-- ~~ Search ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ --
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ --

levenshtein :: (Ord sym, ListLike full sym) => Int -> full -> TSTSet sym
            -> [(full, Distance Levenshtein)]
levenshtein = queryTST (\s -> deletions s ++ replaces s ++ insertions s)

damerauLevenshtein :: (Ord sym, ListLike full sym) => Int -> full -> TSTSet sym
                   -> [(full, Distance DamerauLevenshtein)]
damerauLevenshtein =
    queryTST (\s -> deletions s ++ replaces s ++ insertions s ++ transpositions s)

insert :: (Ord sym, ListLike full sym) => full -> TSTSet sym -> TSTSet sym
insert ll = TSTSet.insert (ListLike.toList ll)
{-# SPECIALISE insert :: String -> TSTSet Char -> TSTSet Char #-}
{-# SPECIALISE insert :: ByteString -> TSTSet Word8 -> TSTSet Word8 #-}
{-# SPECIALISE insert :: Text -> TSTSet Char -> TSTSet Char #-}

queryTST :: (Ord sym, ListLike full sym)
         => (WildList sym -> [WildList sym])
         -> Int -> full -> TSTSet sym -> [(full, Distance algo)]
queryTST f maxd s tst =
    map (first ListLike.fromList) $ TST.toList $
    go 0 TSTSet.empty [wildList $ ListLike.toList s] TST.empty
  where
    go n visited edits matches
        | n <= maxd = let edits'     = filter (\x -> not (TSTSet.member x visited)) edits
                          newMatches = zip (concatMap (flip TSTSet.matchWL tst) edits')
                                           (map Distance (repeat n))
                      in go (n + 1) (foldr TSTSet.insert visited edits')
                            (concatMap f edits') (update newMatches matches)
        | otherwise = matches

    update new matches = foldr (uncurry (TST.insertWith (flip const))) matches new
{-# SPECIALISE queryTST :: (WildList Char -> [WildList Char])
                        -> Int -> String -> TSTSet Char
                        -> [(String, Distance algo)] #-}
{-# SPECIALISE queryTST :: (WildList Word8 -> [WildList Word8])
                        -> Int -> ByteString -> TSTSet Word8
                        -> [(ByteString, Distance algo)] #-}
{-# SPECIALISE queryTST :: (WildList Char -> [WildList Char])
                        -> Int -> Text -> TSTSet Char
                        -> [(Text, Distance algo)] #-}

wildList :: ListLike full sym => full -> WildList sym
wildList = map El . ListLike.toList
