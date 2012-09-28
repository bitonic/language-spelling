module Data.TST
    ( TST
    , empty
    , singleton
    , insert
    , insertWith
    , lookup
    , delete
    , toList
    , fromList

    , WildCard (..)
    , WildList
    , matchWL
    ) where

import            Control.DeepSeq

import            Control.Arrow (first)
import            Prelude hiding (lookup)

-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ --
-- ~~ TST ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ --
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ --

data TST sym v = Branch sym (TST sym v) (TST sym v) (TST sym v)
               | End v (TST sym v)
               | Null

instance (Ord sym, Eq v) => Eq (TST sym v) where
    t1 == t2 = toList t1 == toList t2

instance (Show sym, Ord sym, Show v) => Show (TST sym v) where
    show t1 = "fromList " ++ show (toList t1)

instance (NFData sym, NFData v) => NFData (TST sym v) where
    rnf Null             = ()
    rnf (End v t)        = rnf v `seq` rnf t
    rnf (Branch c l m r) = rnf c `seq` rnf l `seq` rnf m `seq` rnf r

empty :: Ord sym => TST sym v
empty = Null

singleton :: Ord sym => [sym] -> v -> TST sym v
singleton []      v = End v Null
singleton (c : s) v = Branch c Null (singleton s v) Null

insertWith :: Ord sym => (v -> v -> v) -> [sym] -> v -> TST sym v -> TST sym v
insertWith _ s       v  Null             = singleton s v
insertWith f []      v (End v' t)        = End (f v v') t
insertWith f []      v (Branch c l m r)  = Branch c (insertWith f [] v l) m r
insertWith f (c : s) v (End v' t)        = End v' (insertWith f (c : s) v t)
insertWith f (c : s) v (Branch c' l m r) =
    case compare c c' of
        LT -> Branch c' (insertWith f (c : s) v l) m r
        EQ -> Branch c' l (insertWith f s v m) r
        GT -> Branch c' l m (insertWith f (c : s) v  r)

insert :: Ord sym => [sym] -> v -> TST sym v -> TST sym v
insert = insertWith const

lookup :: Ord sym => [sym] -> TST sym v -> Maybe v
lookup _        Null             = Nothing
lookup []      (End v _)         = Just v
lookup []      (Branch _ l _ _)  = lookup [] l
lookup (c : s) (End _ t)         = lookup (c : s) t
lookup (c : s) (Branch c' l m r) = case compare c c' of
                                       LT -> lookup (c : s) l
                                       EQ -> lookup s m
                                       GT -> lookup (c : s) r

-- | We don't need this and it's slow.  But you've got to have a `delete'!
delete :: Ord sym => [sym] -> TST sym v -> TST sym v
delete s0 t0 = go id id t0 s0 t0
  where
    go hole _ root _ Null =
        hole root
    go hole _ root [] (End _ _) =
        hole (fromList (purge root))
    go hole partial _ [] (Branch c l m r) =
        go (\t' -> hole (partial (Branch c t' m r))) id l [] l
    go hole partial root (_ : s) (End v t) =
        go (hole . partial . End v) id root s t
    go hole partial root (c : s) (Branch c' l m r) =
        case compare c c' of
            LT -> go (\t -> hole (partial (Branch c' t m r))) id l (c : s) l
            EQ -> go hole (\t -> partial (Branch c' l t r))  root s m
            GT -> go (\t -> hole (partial (Branch c' l m t))) id r (c : s) r

    -- This can be made faster
    purge  Null            = error "Language.Distance.Search.TST.delete.purge"
    purge (End _ t)        = toList t
    purge (Branch c l m r) = toList l ++ map (first (c :)) (purge m) ++ toList r

toList :: Ord sym => TST sym v -> [([sym], v)]
toList Null             = []
toList (End v t)        = ([], v) : toList t
toList (Branch c l m r) = toList l ++ map (first (c :)) (toList m) ++ toList r

fromList :: Ord sym => [([sym], v)] -> TST sym v
fromList = foldr (uncurry insert) empty

-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ --
-- ~~ Wildcards ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ --
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ --

data WildCard a = WildCard
                | El a
    deriving (Eq, Ord)

instance Show a => Show (WildCard a) where
  show WildCard = "*"
  show (El c)   = show c

type WildList a = [WildCard a]

matchWL :: Ord sym => WildList sym -> TST sym v -> [([sym], v)]
matchWL _        Null             = []
matchWL []      (End v _)         = [([], v)]
matchWL []      (Branch _ l _ _)  = matchWL [] l
matchWL (c : s) (End _ t)         = matchWL (c : s) t
matchWL (c : s) (Branch c' l m r) = let left   = matchWL (c : s) l
                                        middle = map (first (c' :)) (matchWL s m)
                                        right  = matchWL (c : s) r
                                    in case (c, compare c (El c')) of
                                            (WildCard, _ ) -> left ++ middle ++ right
                                            (_,        LT) -> left
                                            (_,        EQ) -> middle
                                            (_,        GT) -> right
