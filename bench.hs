{-# LANGUAGE FlexibleContexts #-}
import Control.Applicative ((<$>))

import           Data.Char (toLower)
import           Data.Monoid (Last (..))
import           Data.Word (Word8)

import           Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as BS

import           Criterion.Main
import           Criterion.Config
import           Data.ListLike (ListLike)
import           System.Random.Shuffle

import           Language.Distance
import           Language.Distance.Search (Search, BKTree, TSTDist)
import qualified Language.Distance.Search as Dist

dict :: IO [String]
dict = (map (map toLower)) . lines <$> readFile "/usr/share/dict/words"

dictBS :: IO [ByteString]
dictBS = (map (BS.map toLower)) . BS.lines <$> BS.readFile "/usr/share/dict/words"

insert :: (ListLike full sym, EditDistance algo sym, Search container sym algo)
       => [full] -> container full sym algo
insert ss = foldr Dist.insert Dist.empty ss

randInsert :: (EditDistance algo Char, Search container Char algo, ListLike full sym)
           => ([full] -> container full sym algo)
           -> [full] -> IO (container full sym algo)
randInsert f ss = f <$> shuffleM ss

insertStringBKLev :: [String] -> BKTree String Char Levenshtein
insertStringBKLev = insert

insertStringBKDamLev :: [String] -> BKTree String Char DamerauLevenshtein
insertStringBKDamLev = insert

insertStringTSTLev :: [String] -> TSTDist String Char Levenshtein
insertStringTSTLev = insert

insertBSBKLev :: [ByteString] -> BKTree ByteString Word8 Levenshtein
insertBSBKLev = insert

insertBSBKDamLev :: [ByteString] -> BKTree ByteString Word8 DamerauLevenshtein
insertBSBKDamLev = insert

insertBSTSTLev :: [ByteString] -> TSTDist ByteString Word8 Levenshtein
insertBSTSTLev = insert

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
          bss <- dictBS
          let config = defaultConfig { cfgReport  = (Last (Just "report.html"))
                                     , cfgSamples = (Last (Just 20))
                                     }
          defaultMainWith config (return ())
              [ bgroup "rand" [ bench "shuffle" $ nfIO (shuffleM ss)]
              , bgroup "string bk"
                    [ bench "insert lev" $ nf insertStringBKLev ss
                    , bench "insert dam-lev" $ nf insertStringBKDamLev ss
                    , bench "insert rand lev" $ nfIO (randInsert insertStringBKLev ss)
                    , bench "insert rand dam-lev" $ nfIO (randInsert insertStringBKDamLev ss)
                    ]
              , bgroup "string tst"
                    [ bench "insert" $ nf insertStringTSTLev ss
                    , bench "insert rand" $ nfIO (randInsert insertStringTSTLev ss)
                    ]
              , bgroup "bytestring bk"
                    [ bench "insert lev" $ nf insertBSBKLev bss
                    , bench "insert dam-lev" $ nf insertBSBKDamLev bss
                    , bench "insert rand lev" $ nfIO (randInsert insertBSBKLev bss)
                    , bench "insert rand dam-lev" $ nfIO (randInsert insertBSBKDamLev bss)
                    ]
              , bgroup "bytestring tst"
                    [ bench "insert" $ nf insertBSTSTLev bss
                    , bench "insert rand" $ nfIO (randInsert insertBSTSTLev bss)
                    ]
           ]