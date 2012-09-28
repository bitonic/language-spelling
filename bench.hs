{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
import Control.Applicative ((<$>))

import           Data.Char (toLower)
import           Data.Monoid (Last (..))
import           Data.Word (Word8)

import           Control.DeepSeq
import           Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as BS

import           Criterion.Main
import           Criterion.Config
import           Data.ListLike (ListLike)
import           System.Random.Shuffle

import           Language.Distance
import           Language.Distance.Search (Search, BKTree, TSTDist)
import qualified Language.Distance.Search as Dist

everyN :: Int -> [a] -> [a]
everyN n xs = case drop n xs of
                  []        -> []
                  (x : xs') -> x : everyN n xs'

dict :: IO [String]
dict = (map (map toLower)) . everyN 20 . lines <$> readFile "/usr/share/dict/words"

dictBS :: IO [ByteString]
dictBS = (map (BS.map toLower)) . everyN 20 . BS.lines <$> BS.readFile "/usr/share/dict/words"

insert :: (ListLike full sym, EditDistance algo sym, Search container sym algo)
       => [full] -> container full sym algo
insert ss = foldr Dist.insert Dist.empty ss

query :: (ListLike full sym, EditDistance algo sym, Search container sym algo)
      => Int -> [full] -> container full sym algo -> ()
query n ss dist = length [() | s <- ss, length (Dist.query n s dist) > 0] `seq` ()

group1 :: forall container full sym algo.
          ( NFData (container full sym algo)
          , ListLike full sym
          , EditDistance algo sym
          , Search container sym algo
          ) => container full sym algo
            -> [full] -> [full] -> [full] -> String -> [Benchmark]
group1 _ ss rand1ss rand2ss name =
    dist `deepseq` distRand `deepseq`
    [ bench ("insert " ++ name) $
      nf (insert :: [full] -> container full sym algo) ss
    , bench ("insert rand " ++ name) $
      nf (insert :: [full] -> container full sym algo) rand1ss
    , bench ("lookup " ++ name) $
      nf (query 0 rand2ss :: container full sym algo -> ()) dist
    , bench ("lookup rand " ++ name) $
      nf (query 0 rand2ss :: container full sym algo -> ()) distRand
    ]
  where
    dist     = Dist.fromList ss
    distRand = Dist.fromList rand1ss

group2 :: forall container.
          ( NFData (container String Char Levenshtein)
          , NFData (container ByteString Word8 Levenshtein)
          , NFData (container String Char DamerauLevenshtein)
          , NFData (container ByteString Word8 DamerauLevenshtein)
          , Search container Char Levenshtein
          , Search container Word8 Levenshtein
          , Search container Char DamerauLevenshtein
          , Search container Word8 DamerauLevenshtein
          ) => container String Char Levenshtein
            -> [String] -> [String] -> [String]
            -> [ByteString] -> [ByteString] -> [ByteString]
            -> String -> [Benchmark]
group2 _ ss rand1ss rand2ss bss rand1bss rand2bss name =
    [ bgroup (name ++ " string") $
      (group1 (undefined :: container String Char Levenshtein)
              ss rand1ss rand2ss "lev") ++
      (group1 (undefined :: container String Char DamerauLevenshtein)
              ss rand1ss rand2ss "dam-lev")
    , bgroup (name ++ " bytestring") $
      (group1 (undefined :: container ByteString Word8 Levenshtein)
              bss rand1bss rand2bss "lev") ++
      (group1 (undefined :: container ByteString Word8 DamerauLevenshtein)
              bss rand1bss rand2bss "dam-lev")
    ]

main :: IO ()
main = do ss       <- dict
          rand1ss  <- shuffleM ss
          rand2ss  <- shuffleM ss
          bss      <- dictBS
          rand1bss <- shuffleM bss
          rand2bss <- shuffleM bss
          let config = defaultConfig { cfgReport  = (Last (Just "report.html"))
                                     , cfgSamples = (Last (Just 100))
                                     }
          defaultMainWith config (return ())
                          (group2 (undefined :: TSTDist full sym algo)
                                  ss rand1ss rand2ss bss rand1bss rand2bss "tst" ++
                           group2 (undefined :: BKTree full sym algo)
                                  ss rand1ss rand2ss bss rand1bss rand2bss "bk")
