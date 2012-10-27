{-# LANGUAGE CPP #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
import Control.Applicative ((<$>))

import           Data.Char (toLower)
import           Data.Monoid (Last (..))

import           Control.DeepSeq (NFData)
import qualified Data.ByteString.Char8 as ByteString
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import           Data.Time.Clock.POSIX (getPOSIXTime)

import           Criterion.Config
import           Criterion.Main
import           Data.ListLike.Text ()
import           System.Random.Shuffle

import           Language.Distance.Internal (Distance (..))
import qualified Language.Distance.Search.BK as BK
import qualified Language.Distance.Search.TST as TST


-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ --
-- ~~ Utils and dictionaries ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ --
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ --

everyN n xs = case drop n xs of
                  []        -> []
                  (x : xs') -> x : everyN n xs'

-- We get every 20 words since testing with the whole dict is too much
dict m lin read_ = (map (m toLower)) . everyN 20 . lin <$> read_ "/usr/share/dict/words"

dictS = dict map lines readFile

dictBS = dict ByteString.map ByteString.lines ByteString.readFile

dictT = dict Text.map Text.lines Text.readFile

-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ --
-- ~~ Search ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ --
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ --

deriving instance NFData (Distance algo)

query queryc n ss dist = map (\s -> queryc n s dist) ss

group1 emptyc insertc queryc ss rand1ss rand2ss name =
    dist `seq` distRand `seq`
    [ bench ("insert " ++ name) $ whnf (foldr insertc emptyc) ss
    , bench ("insert rand " ++ name) $ whnf (foldr insertc emptyc) rand1ss
    , bench ("lookup " ++ name) $ nf (query queryc 0 rand2ss) dist
    , bench ("lookup rand " ++ name) $ nf (query queryc 0 rand2ss) distRand
    , bench ("query 1 " ++ name) $ nf (query queryc 1 (take 100 rand2ss)) dist
    , bench ("query 1 rand " ++ name) $ nf (query queryc 1 (take 100 rand2ss)) distRand
    , bench ("query 2 " ++ name) $ nf (query queryc 2 (take 100 rand2ss)) dist
    , bench ("query 2 rand " ++ name) $ nf (query queryc 2 (take 100 rand2ss)) distRand
    ]
  where
    dist     = foldr insertc emptyc ss
    distRand = foldr insertc emptyc rand1ss

#define group2(NAME, TYPE, SS, RAND1SS, RAND2SS, EMPTY, INSERT, LEVEN, DAMLEV) \
    (bgroup (NAME ++ " " ++ TYPE) \
     ((group1 EMPTY INSERT LEVEN SS RAND1SS RAND2SS "lev") ++ \
      (group1 EMPTY INSERT LEVEN SS RAND1SS RAND2SS "dam-lev")))

group3 ss rand1ss rand2ss =
    [ group2("tst", "string", ss, rand1ss, rand2ss, TST.empty, TST.insert,
             TST.levenshtein, TST.damerauLevenshtein)
    , group2("tst", "bytestring", bss, rand1bss, rand2bss, TST.empty, TST.insert,
             TST.levenshtein, TST.damerauLevenshtein)
    , group2("tst", "text", ts, rand1ts, rand2ts, TST.empty, TST.insert, TST.levenshtein,
             TST.damerauLevenshtein)
    , group2("bk", "string", ss, rand1ss, rand2ss, BK.empty, BK.insert, BK.levenshtein,
             BK.damerauLevenshtein)
    , group2("bk", "bytestring", bss, rand1bss, rand2bss, BK.empty, BK.insert,
             BK.levenshtein, BK.damerauLevenshtein)
    , group2("bk", "text", ts, rand1ts, rand2ts, BK.empty, BK.insert, BK.levenshtein,
             BK.damerauLevenshtein)
    ]
  where
    bss      = map ByteString.pack ss
    rand1bss = map ByteString.pack rand1ss
    rand2bss = map ByteString.pack rand2ss
    ts       = map Text.pack ss
    rand1ts  = map Text.pack rand1ss
    rand2ts  = map Text.pack rand2ss

-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ --
-- ~~ Distance ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ --
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ --

main = do ss       <- dictS
          rand1ss  <- shuffleM ss
          rand2ss  <- shuffleM ss
          fn       <- (++ ".html") . (show :: Integer -> String) . ceiling <$>
                      getPOSIXTime
          let config = defaultConfig { cfgReport  = (Last (Just fn))
                                     , cfgSamples = (Last (Just 100))
                                     }
          defaultMainWith config (return ()) (group3 ss rand1ss rand2ss)
