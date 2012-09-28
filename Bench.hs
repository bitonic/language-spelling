{-# LANGUAGE FlexibleContexts #-}
module Bench (main) where
import Control.Applicative ((<$>))

import           Data.Char (toLower)
import           Data.Monoid (Last (..))

import           Criterion.Main
import           Criterion.Config
import           Data.ListLike (ListLike)
import           System.Random.Shuffle

import           Data.TST (WildCard (..), WildList)
import           Language.Distance
import           Language.Distance.Search (Search, BKTree, TSTDist)
import qualified Language.Distance.Search as Dist

dict :: IO [String]
dict = (map (map toLower)) . lines <$> readFile "/usr/share/dict/words"

insert :: (EditDistance algo Char, Search container Char algo)
       => [String] -> container String Char algo
insert ss = foldr Dist.insert Dist.empty ss

randInsert :: (EditDistance algo Char, Search container Char algo)
           => ([String] -> container String Char algo)
           -> [String] -> IO (container String Char algo)
randInsert f ss = f <$> shuffleM ss

insertBKLev :: [String] -> BKTree String Char Levenshtein
insertBKLev = insert

insertBKDamLev :: [String] -> BKTree String Char DamerauLevenshtein
insertBKDamLev = insert

insertTSTLev :: [String] -> TSTDist String Char Levenshtein
insertTSTLev = insert

-- scrambleRepl :: [Char]
-- scrambleRepl = ['a'..'z']

-- scrambleWL :: (WildList Char -> [WildList Char]) -> String -> Int -> String
-- scrambleWL = undefined

-- scrambleLev :: String -> Int -> [String]
-- scrambleLev = undefined

-- scrambleDamLev :: String -> Int -> [String]
-- scrambleDamLev = undefined

main :: IO ()
main = do ss <- dict
          let config = defaultConfig { cfgReport  = (Last (Just "report.html"))
                                     , cfgSamples = (Last (Just 100))
                                     }
          defaultMainWith config (return ())
              [ bgroup "rand" [ bench "shuffle" $ nfIO (shuffleM ss)]
              , bgroup "bk"
                    [ bench "insert lev" $ nf insertBKLev ss
                    , bench "insert dam-lev" $ nf insertBKDamLev ss
                    , bench "insert rand lev" $ nfIO (randInsert insertBKLev ss)
                    , bench "insert rand dam-lev" $ nfIO (randInsert insertBKDamLev ss)
                    ]
              , bgroup "tst"
                    [ bench "insert" $ nf insertTSTLev ss
                    , bench "insert rand" $ nfIO (randInsert insertTSTLev ss)
                    ]
           ]