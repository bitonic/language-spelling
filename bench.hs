{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
import Control.Applicative ((<$>))

import           Data.Char (toLower)
import           Data.Monoid (Last (..))
import           Data.Word (Word8)

import           Control.DeepSeq
import           Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as BS
import           Data.Time.Clock.POSIX (getPOSIXTime)

import           Criterion.Main
import           Criterion.Config
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

insert :: Search container full algo => [full] -> container
insert ss = foldr Dist.insert Dist.empty ss

query :: (Search container full algo) => Int -> [full] -> container -> ()
query n ss dist = length [() | s <- ss, length (Dist.query n s dist) > 0] `seq` ()

group1 :: forall container full algo. (NFData container, Search container full algo)
       => container -> [full] -> [full] -> [full] -> String -> [Benchmark]
group1 _ ss rand1ss rand2ss name =
    dist `deepseq` distRand `deepseq`
    [ bench ("insert " ++ name) $
      nf (insert :: [full] -> container) ss
    , bench ("insert rand " ++ name) $
      nf (insert :: [full] -> container) rand1ss
    , bench ("lookup " ++ name) $
      nf (query 0 rand2ss :: container -> ()) dist
    , bench ("lookup rand " ++ name) $
      nf (query 0 rand2ss :: container -> ()) distRand
    , bench ("query 1 " ++ name) $
      nf (query 1 (take 100 rand2ss) :: container -> ()) dist
    , bench ("query 1 rand " ++ name) $
      nf (query 1 (take 100 rand2ss) :: container -> ()) distRand
    , bench ("query 2 " ++ name) $
      nf (query 2 (take 100 rand2ss) :: container -> ()) dist
    , bench ("query 2 rand " ++ name) $
      nf (query 2 (take 100 rand2ss) :: container -> ()) distRand
    ]
  where
    dist     = Dist.fromList ss
    distRand = Dist.fromList rand1ss

-- yuk
group2
    :: forall container full sym algo.
       ( NFData (container String Char Levenshtein)
       , NFData (container ByteString Word8 Levenshtein)
       , NFData (container String Char DamerauLevenshtein)
       , NFData (container ByteString Word8 DamerauLevenshtein)
       , Search (container String Char Levenshtein) String Levenshtein
       , Search (container ByteString Word8 Levenshtein) ByteString Levenshtein
       , Search (container String Char DamerauLevenshtein) String DamerauLevenshtein
       , Search (container ByteString Word8 DamerauLevenshtein) ByteString DamerauLevenshtein
       ) => container full sym algo -> [String] -> [String] -> [String]
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
          fn       <- (++ ".html") . (show :: Integer -> String) . ceiling <$>
                      getPOSIXTime
          let config = defaultConfig { cfgReport  = (Last (Just fn))
                                     , cfgSamples = (Last (Just 100))
                                     }
          defaultMainWith config (return ())
                          (group2 (undefined :: TSTDist full sym algo)
                                  ss rand1ss rand2ss bss rand1bss rand2bss "tst" ++
                           group2 (undefined :: BKTree full sym algo)
                                  ss rand1ss rand2ss bss rand1bss rand2bss "bk")
