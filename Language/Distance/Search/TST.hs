{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Language.Distance.Search.TST
    ( TST
    , empty
    , singleton
    , insert
    , member
    , delete
    , toList
    , fromList

    , TSTDist
    ) where

import           Control.Arrow (first)

import           Data.HashMap.Lazy (HashMap)
import qualified Data.HashMap.Lazy as HashMap
import           Data.Hashable
import           Data.ListLike (ListLike)
import qualified Data.ListLike as ListLike

import           Language.Distance
import           Language.Distance.Internal
import           Language.Distance.Search.Class (Search)
import qualified Language.Distance.Search.Class as Class

-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ --
-- ~~ TST ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ --
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ --

data TST sym = Branch sym (TST sym) (TST sym) (TST sym)
             | End (TST sym)
             | Null

instance Ord sym => Eq (TST sym) where
    t1 == t2 = toList t1 == toList t2

instance (Show sym, Ord sym) => Show (TST sym) where
    show t1 = "fromList " ++ show (toList t1)

empty :: TST sym
empty = Null

singleton :: [sym] -> TST sym
singleton []      = End Null
singleton (c : s) = Branch c Null (singleton s) Null

insert :: Ord sym => [sym] -> TST sym -> TST sym
insert s          Null             = singleton s
insert []      t@(End _  )         = t
insert []        (Branch c l m r)  = Branch c (insert [] l) m r
insert (c : s)   (End t)           = End (insert (c : s) t)
insert (c : s)   (Branch c' l m r) = case compare c c' of
                                         LT -> Branch c' (insert (c : s) l) m r
                                         EQ -> Branch c' l (insert s m) r
                                         GT -> Branch c' l m (insert (c : s)  r)

member :: Ord sym => [sym] -> TST sym -> Bool
member _        Null             = False
member []      (End _)           = True
member []      (Branch _ l _ _)  = member [] l
member (c : s) (End t)           = member (c : s) t
member (c : s) (Branch c' l m r) = case compare c c' of
                                       LT -> member (c : s) l
                                       EQ -> member s m
                                       GT -> member (c : s) r

delete :: Ord sym => [sym] -> TST sym -> TST sym
delete s0 t0 = go id id t0 s0 t0
  where
    go hole _ root _ Null =
        hole root
    go hole _ root [] (End _) =
        hole (fromList (purge root))
    go hole partial _ [] (Branch c l m r) =
        go (\t' -> hole (partial (Branch c t' m r))) id l [] l
    go hole partial root (_ : s) (End t) =
        go (hole . partial . End) id root s t
    go hole partial root (c : s) (Branch c' l m r) =
        case compare c c' of
            LT -> go (\t -> hole (partial (Branch c' t m r))) id l (c : s) l
            EQ -> go hole (\t -> partial (Branch c' l t r))  root s m
            GT -> go (\t -> hole (partial (Branch c' l m t))) id r (c : s) r

    -- This can be made faster
    purge  Null            = error "Language.Distance.Search.TST.delete.purge"
    purge (End t)          = toList t
    purge (Branch c l m r) = toList l ++ map (c :) (purge m) ++ toList r

toList :: Ord sym => TST sym -> [[sym]]
toList Null             = []
toList (End t)          = [] : toList t
toList (Branch c l m r) = toList l ++ map (c :) (toList m) ++ toList r

fromList :: Ord sym => [[sym]] -> TST sym
fromList = foldr insert empty

-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ --
-- ~~ Wildcards ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ --
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ --

data Wildcard a = Wildcard
                | El a
    deriving (Eq, Ord)

instance Show a => Show (Wildcard a) where
  show Wildcard = "*"
  show (El c)   = show c

type WildList a = [Wildcard a]

wildList :: ListLike full sym => full -> WildList sym
wildList = map El . ListLike.toList

-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ --
-- ~~ Edits ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ --
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ --

deleted, transposes, replaces, inserts :: WildList a -> [WildList a]

deleted []      = []
deleted (c : s) = s : map (c :) (deleted s)

transposes []  = []
transposes [x] = [[x]]
transposes (x : y : s) = (y : x : s) : map (x :) (transposes (y : s))

replaces [] = []
replaces (c : s) = (Wildcard : s) : map (c :) (replaces s)

inserts [] = [[Wildcard]]
inserts (c : s) = (Wildcard : c : s) : map (c :) (inserts s)

matchWL' :: Ord sym => (a -> sym) -> WildList sym -> TST a -> [[sym]]
matchWL' _ _        Null             = []
matchWL' _ []      (End _)           = [[]]
matchWL' f []      (Branch _ l _ _)  = matchWL' f [] l
matchWL' f (c : s) (End t)           = matchWL' f (c : s) t
matchWL' f (c : s) (Branch c' l m r) = let left   = matchWL' f (c : s) l
                                           middle = map (f c' :) (matchWL' f s m)
                                           right  = matchWL' f (c : s) r
                                       in case (c, compare c (El $ f c')) of
                                               (Wildcard, _ ) -> left ++ middle ++ right
                                               (_,        LT) -> left
                                               (_,        EQ) -> middle
                                               (_,        GT) -> right

matchWL :: Ord sym => WildList sym -> TST sym -> [[sym]]
matchWL = matchWL' id

iterateEdits :: (Eq a, Ord a) => ([a] -> [[a]]) -> [a] -> Int -> [([a], Distance algo)]
iterateEdits f start iter = undefined

-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ --
-- ~~ TST Search ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ --
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ --

newtype TSTDist full sym algo = TSTDist {getTST :: TST sym}

instance (Ord sym, Hashable sym) => Search TSTDist sym Levenshtein where
    empty     = TSTDist empty
    insert ll = TSTDist . insert (ListLike.toList ll) . getTST

    query = queryTST (\s -> deleted s ++ replaces s ++ inserts s)

instance (Ord sym, Hashable sym) => Search TSTDist sym DamerauLevenshtein where
    empty     = TSTDist empty
    insert ll = TSTDist . insert (ListLike.toList ll) . getTST

    query = queryTST (\s -> deleted s ++ replaces s ++ inserts s ++ transposes s)

queryTST :: (Ord item, ListLike full item, Hashable item)
         => ([Wildcard item] -> [[Wildcard item]])
         -> Int -> full -> TSTDist full item algo -> [(full, Distance algo)]
queryTST edits i ll (TSTDist tst) =
        map (first ListLike.fromList) $ HashMap.toList $ fromListMax $
        concatMap (\(wc, dist) -> zip (matchWL wc tst) (repeat dist))
                  (iterateEdits edits (wildList ll) i)

fromListMax :: (Eq k, Hashable k, Ord v) => [(k, v)] -> HashMap k v
fromListMax []              = HashMap.empty
fromListMax ((k, v) : rest) = HashMap.insertWith min k v (fromListMax rest)