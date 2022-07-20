{-# LANGUAGE DeriveTraversable #-}
module RegExp.AST
  ( RegF(..), RegExp(..), SetMode(..), RegFullF(..), RegExpFull(..),
    empty, one, oneOf, noneOf, anyone, (|||), (>>>), rep, backref,
    repExact, repAtLeast, repBetween,
    grouping, foldRegExp, foldRegExpFull, simplify,
    mapRegF,
    AcceptsEmpty(..)
  ) where

data SetMode = InSet | NotInSet
  deriving (Read,Show,Eq,Ord)

data RegF e a r
  = Empty
  | OneOf SetMode [a]
  | Rep r
  | Alt e r r -- the Bool caches if accept empty
  | Seq e r r -- ditto
    deriving (Read,Show,Functor,Foldable,Traversable)

-- | This type adds additional support for backreferences to
-- a regular expression
data RegFullF a r
  = RegF (RegF () a r)
  | Group r
  | BackRef Int
    deriving (Read,Show,Functor,Foldable,Traversable)

class AcceptsEmpty t where
  acceptsEmpty :: t -> Bool

instance AcceptsEmpty Bool where acceptsEmpty = id

instance AcceptsEmpty e => AcceptsEmpty (RegF e a r) where
  acceptsEmpty r =
    case r of
      Empty     -> True
      OneOf {}  -> False
      Rep {}    -> True
      Alt b _ _ -> acceptsEmpty b
      Seq b _ _ -> acceptsEmpty b


newtype RegExp a = RE (RegF Bool a (RegExp a))
                      deriving (Read,Show)

newtype RegExpFull a = REF (RegFullF a (RegExpFull a))
                      deriving (Read,Show)

instance Functor RegExpFull where
  fmap f (REF r) = REF (mapRegFullF f r)

mapRegFullF :: Functor f => (a -> b) -> RegFullF a (f a) -> RegFullF b (f b)
mapRegFullF f r =
    case r of
      Group a   -> Group (fmap f a)
      BackRef i -> BackRef i
      RegF a    -> RegF (mapRegF f a)

instance Functor RegExp where
  fmap f (RE r) = RE (mapRegF f r)

mapRegF :: Functor f => (a -> b) -> RegF e a (f a) -> RegF e b (f b)
mapRegF f r =
    case r of
      Empty     -> Empty
      OneOf m x -> OneOf m (map f x)
      Rep p     -> Rep (fmap f p)
      Alt b p q -> Alt b (fmap f p) (fmap f q)
      Seq b p q -> Seq b (fmap f p) (fmap f q)

foldRegExp :: (RegF Bool a r -> r) -> RegExp a -> r
foldRegExp f (RE x) = f (fmap (foldRegExp f) x)

foldRegExpFull :: (RegFullF a r -> r) -> RegExpFull a -> r
foldRegExpFull f (REF x) = f (fmap (foldRegExpFull f) x)

instance AcceptsEmpty (RegExp a) where
  acceptsEmpty (RE e) = acceptsEmpty e

simple :: RegF () a (RegExpFull a) -> RegExpFull a
simple = REF . RegF

empty    :: RegExpFull a
empty     = simple Empty

one      :: a -> RegExpFull a
one a     = simple (OneOf InSet [a])

oneOf    :: [a] -> RegExpFull a
oneOf xs  = simple (OneOf InSet xs)

noneOf   :: [a] -> RegExpFull a
noneOf xs = simple (OneOf NotInSet xs)

anyone   :: RegExpFull a
anyone    = noneOf []

(|||)    :: RegExpFull a -> RegExpFull a -> RegExpFull a
r1 ||| r2 = simple (Alt () r1 r2)

(>>>)    :: RegExpFull a -> RegExpFull a -> RegExpFull a
r1 >>> r2 = simple (Seq () r1 r2)

rep      :: RegExpFull a -> RegExpFull a
rep r     = simple (Rep r)

seqs :: [RegExpFull a] -> RegExpFull a
seqs [] = empty
seqs xs = foldr1 (>>>) xs

repExact :: Int -> RegExpFull a -> RegExpFull a
repExact n m = seqs (replicate n m)

repAtLeast :: Int -> RegExpFull a -> RegExpFull a
repAtLeast n m = seqs (replicate n m ++ [rep m])

repBetween :: Int -> Int -> RegExpFull a -> RegExpFull a
repBetween n1 n2 m | n1 == n2 = repExact n1 m
repBetween n1 n2 m = seqs (replicate n1 m ++ [repUpTo (n2-n1) m])

repUpTo :: Int -> RegExpFull a -> RegExpFull a
repUpTo 1 m = empty ||| m
repUpTo n m = empty ||| (m >>> repUpTo (n-1) m)

grouping :: RegExpFull a -> RegExpFull a
grouping r = REF (Group r)

backref :: Int -> RegExpFull a
backref i = REF (BackRef i)

simplify :: RegExpFull a -> Maybe (RegExp a)
simplify = foldRegExpFull $ \r ->
  case r of
    Group g   -> g
    BackRef{} -> Nothing
    RegF s    -> RE . cacheAcceptsEmpty <$> sequence s

cacheAcceptsEmpty :: AcceptsEmpty r => RegF z a r -> RegF Bool a r
cacheAcceptsEmpty r =
  case r of
    Empty     -> Empty
    OneOf m x -> OneOf m x
    Rep p     -> Rep p
    Alt _ p q -> Alt (acceptsEmpty p || acceptsEmpty q) p q
    Seq _ p q -> Seq (acceptsEmpty p && acceptsEmpty q) p q
