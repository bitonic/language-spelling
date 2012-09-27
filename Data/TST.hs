module Data.TST
    ( TST
    , empty
    , singleton
    , insert
    , member
    , delete
    , toList
    , fromList

    , WildCard (..)
    , WildList
    , matchWL
    ) where

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

-- | We don't need this and it's slow.  But you've got to have a `delete'!
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

data WildCard a = WildCard
                | El a
    deriving (Eq, Ord)

instance Show a => Show (WildCard a) where
  show WildCard = "*"
  show (El c)   = show c

type WildList a = [WildCard a]

matchWL :: Ord sym => WildList sym -> TST sym -> [[sym]]
matchWL _        Null             = []
matchWL []      (End _)           = [[]]
matchWL []      (Branch _ l _ _)  = matchWL [] l
matchWL (c : s) (End t)           = matchWL (c : s) t
matchWL (c : s) (Branch c' l m r) = let left   = matchWL (c : s) l
                                        middle = map (c' :) (matchWL s m)
                                        right  = matchWL (c : s) r
                                    in case (c, compare c (El c')) of
                                            (WildCard, _ ) -> left ++ middle ++ right
                                            (_,        LT) -> left
                                            (_,        EQ) -> middle
                                            (_,        GT) -> right
