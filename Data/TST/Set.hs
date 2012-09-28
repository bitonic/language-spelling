module Data.TST.Set
    ( TSTSet
    , empty
    , singleton
    , insert
    , member
    , delete
    , toList
    , fromList
    , matchWL
    ) where

import           Prelude hiding (lookup)

import           Data.TST (TST, WildList)
import qualified Data.TST as TST

newtype TSTSet sym = TSTSet {unTSTSet :: TST sym ()}

instance Ord sym => Eq (TSTSet sym) where
    t1 == t2 = toList t1 == toList t2

instance (Show sym, Ord sym) => Show (TSTSet sym) where
    show t1 = "fromList " ++ show (toList t1)

empty :: Ord sym => TSTSet sym
empty = TSTSet TST.empty

singleton :: Ord sym => [sym] -> TSTSet sym
singleton s = TSTSet $ TST.singleton s ()

insert :: Ord sym => [sym] -> TSTSet sym -> TSTSet sym
insert s = TSTSet . TST.insert s () . unTSTSet

member :: Ord sym => [sym] -> TSTSet sym -> Bool
member s (TSTSet tst) = TST.lookup s tst == Just ()

delete :: Ord sym => [sym] -> TSTSet sym -> TSTSet sym
delete s = TSTSet . TST.delete s . unTSTSet

toList :: Ord sym => TSTSet sym -> [[sym]]
toList = map fst . TST.toList . unTSTSet

fromList :: Ord sym => [[sym]] -> TSTSet sym
fromList = TSTSet . TST.fromList . map (\s -> (s, ()))

matchWL :: Ord sym => WildList sym -> TSTSet sym -> [[sym]]
matchWL s = map fst . TST.matchWL s . unTSTSet
