import Control.Applicative
import Control.Monad.Trans.State

-- *** Functors made for walking *** ---
--
-- Analogous: foldr with applicative!
-- traverse :: (Applicative f, Traversable t) => (a -> f b) -> t a -> f (t b)
--   where (a -> f b) --> the lifter which lifts each element (a) into applicative (f b);
--                        during folding, a operation (g) gets lifted into applicative context (f),
--                        and used as fold-node in the fold-tree, where each leaf is a (f b)
--         (t a)      --> the structure to be traversed
--         (f (t b))  --> the resulted structure (t b) after traversal, which is only valid in
--                        the applicative context (f), since operations generating it are nested
--                        in (f)

{- Definition of Traversable typeclass 

class (Functor t, Foldable t) => Traversable t where
  traverse  :: Applicative f => (a -> f b) -> t a -> f (t b)
  sequenceA :: Applicative f => t (f a) -> f (t a)

  traverse f = sequenceA . fmap f
  sequenceA = traverse id

  -- spec with Monad
  mapM      :: Monad m => (a -> m b) -> t a -> m (t b)
  sequence  :: Monad m => t (m a) -> m (t a)

instance Traversable [] where

  sequenceA :: Applicative f => [f a] -> f [a]
  sequenceA []       = pure []
  sequenceA (fa:fas) = (:) <$> fa <*> sequenceA fas

  -- OR

  traverse :: Applicative f => (a -> f b) -> [a] -> f [b]
  traverse _ []     = pure []
  traverse g (a:as) = (:) <$> (g a) <*> traverse g as
  
  -- !! foldr with Applicative !!
-}

assertNegative :: (Num a, Ord a) => a -> Maybe a
assertNegative x | x < 0     = Nothing
                 | otherwise = Just x

rejectNegative :: (Num a, Ord a) => [a] -> Maybe [a]
-- rejectNegative = traverse assertNegative
-- sequenceA :: t = [], f = Maybe => [Just a] -> Just [a]
rejectNegative = sequenceA . fmap assertNegative


data Tree a = Leaf | Branch a (Tree a) (Tree a)

instance Functor Tree where
  fmap _ Leaf = Leaf
  fmap f (Branch a l r) = Branch (f a) (fmap f l) (fmap f r)

instance Foldable Tree where
  -- foldMap :: (Monoid m, Foldable t) => (a -> m) -> t a -> m
  -- foldMap :: (Monoid m) => (a -> m) -> Tree a -> m
  foldMap _ Leaf = mempty
  foldMap h (Branch a l r) = (foldMap h l) `mappend` h a `mappend` (foldMap h r)

instance Traversable Tree where
  -- traverse :: Applicative f => (a -> f b) -> Tree a -> f (Tree b)
  traverse _ Leaf = pure Leaf
  traverse h (Branch a l r) = Branch <$> h a <*> traverse h l <*> traverse h r


xss = traverse (\x -> [0..x]) [0..3]
-- traverse f (0:1:2:3:[])
-- == (:) <$> (f 0) <*> traverse (1:2:3:[])
-- == (:) <$> (f 0) <*> ((:) <$> (f 1) <*> (2:3:[]))
-- == (:) <$> [0..0] <*> ((:) <$> [0..1] <*> (... ( (:) <$> [0..3] <*> pure [])))


-- * Ex. Matrix transposition

matTrans :: [[a]] -> [[a]]
matTrans [] = []
matTrans [r] = pure <$> r
matTrans (r:rs) = zipWith (:) r $ matTrans rs

matTrans1 :: [[a]] -> [[a]]
matTrans1 ([]:_) = []
matTrans1 rs = (map head rs) : (matTrans1 $ map tail rs)


transpose1 :: [[a]] -> [[a]]
-- traverse ZipList [] = ZipList []
-- traverse ZipList (r:rs) = (:) <$> ZipList r <*> traverse ZipList rs
transpose1 = getZipList . traverse ZipList


-- * Ex. traverse mappend

tm :: (Traversable t, Monoid b) => t b -> b -> t b
tm = traverse mappend
     -- traverse :: (a -> f b) -> t a -> f (t b)
     --          :: Monoid b => (a -> b) -> t a -> t b
     -- mappend :: Monoid b => b -> b -> b

-- For lists
-- mappend  :: [a] -> [a] -> [a]
-- let a == [a]
--     b == [a]
--     t == []
--     t a == [[a]]
--     t b == [[a]]
--     f == ([a] ->)
-- traverse :: ([a] -> ([a] -> [a])) -> [[a]] -> ([a] -> [[a]])
-- traverse mappend :: [[a]] -> ([a] -> [[a]])

-- traverse mappend [] == \as -> [as]
-- traverse mappend (as:ass) = (:) <$> (mappend as) <*> traverse mappend ass

{- RECALL:
instance Applicative (x ->) where

  pure :: a -> (x -> a)
  pure a = \x -> a

  (<*>) :: ((x -> a) -> (x -> b)) -> (x -> a) -> (x -> b)
  xf <*> xa = \x -> (xf x) (xa x)

  (<$>) :: (a -> b) -> (x -> a) -> (x -> b)
  (<$>) = fmap

-- Specialized with the (x ->) functor, the (<*>) operator constructs
-- a function (\x -> ...), which first feeds/distributes the argument
-- (x) to BOTH sides.

-- Treating the structure (f <$> x1 <*> (f <$> x2 <*> ( ... ))) like
-- folded structure (g x1 (g x2 ( ... ))), the same behavior
-- i.e. feeding (x) is mapped to each resulted component corresponding
-- to x1, x2, ... etc.
-}

-- * Ex. mapAccumL :: 
-- mapAccumL :: (a -> b -> (a, c)) -> a -> [b] -> (a, [c])

-- mAL f a []     = (a, [])
-- mAL f a (b:bs) =
--   let
--     (a1, c) = f a b
--     (a2, cs) = mAL f a1 bs
--   in
--     (a2, c:cs)

-- Flip the type expression into...
-- (b -> a -> (a, c)) -> t b -> a -> (a, t c)
-- to match the (State) stuff...
-- (b -> State a c) -> t b -> State a (t c)

flipStateFunc :: (a -> (a, c)) -> (a -> (c, a))
flipStateFunc = fmap (\(a,c) -> (c,a))

mapAccumLS :: Traversable t => (b -> State a c) -> t b -> State a (t c)
-- traverse :: (x -> f y) -> t x -> f (t y)
-- SPEC with x = b
--           f = (State a) where (State a c = a -> (c, a))
--           y = c
-- traverse :: (b -> State a c) -> t b -> State a (t c)
-- mapAccumLS step bs = traverse step bs
mapAccumLS = traverse

-- Flip back!
mapAccumLF :: Traversable t => (a -> b -> (c, a)) -> a -> t b -> (t c, a)
mapAccumLF step z tb = runState (traverse (state . flip step) tb) z
-- BUT here it is (c,a) not (a,c) as mapAccumL requires!

-- Write my own "flipped" version of State
newtype MyState s a = MyState { runMyState :: s -> (s, a) }

instance Functor (MyState s) where
  fmap f (MyState r) = MyState $ \s -> let (s1, a) = r s in (s1, f a)
instance Applicative (MyState s) where
  pure a = MyState $ \s -> (s, a)
  (MyState rf) <*> (MyState r) = MyState $ \s -> let (s1, f) = rf s
                                                     (s2, a) = r s1
                                                 in (s2, f a)

mapAccumL1 :: (a -> b -> (a, c)) -> a -> [b] -> (a, [c])
mapAccumL1 step z tb = runMyState (traverse (MyState . flip step) tb) z


-- *** (Traversable) laws ***

{- *
traverse :: Applicative f, Traversable t => (a -> f b) -> t a -> f (t b)

traverse Identity == Identity

-}

newtype Compose f g a = Compose { getCompose :: f (g a) }
instance (Functor f, Functor g) => Functor (Compose f g) where
  fmap f (Compose x) = Compose $ fmap (fmap f) x
instance (Applicative f, Applicative g) => Applicative (Compose f g) where
  pure x = Compose $ pure (pure x)
  -- LHS
  -- (<*>) :: f (a -> b) -> (f a -> f b)
  -- SPEC f = Compose f g
  -- (<*>) :: Compose f g (a -> b) -> Compose f g a -> Compose f g b
  --
  -- RHS
  -- fgh :: f (g (a -> b))
  -- fga :: f (g a)
  -- (<*>) :: g (a -> b) -> g a -> g b
  -- ((<*>) <$>) == fmap (<*>) :: Functor f => f (g (a -> b)) -> f (g a) -> f (g b)
  Compose fgh <*> Compose fga = Compose $ (<*>) <$> fgh <*> fga

{- * With Compose...

traverse (Compose . fmap g . f) == Compose . fmap . (traverse g) . traverse f

-}

{- * in terms of (sequenceA)

sequenceA . fmap Identity = Idendity
-}



-- *** Recovering fmap and foldMap ***

-- fmap :: (a -> b) -> f a -> f b
newtype Identity a = Identity { runIdentity :: a }
instance Functor Identity where
  fmap f = Identity . f . runIdentity
instance Applicative Identity where
  pure = Identity
  (<*>) (Identity f) = fmap f

fmap' :: Traversable f => (a -> b) -> (f a -> f b)
-- (Identity . f) :: a -> f b    -- The common trans-lifter like (a -> m b)
fmap' f = runIdentity . traverse (Identity . f)


-- foldMap :: (Monoid m, Foldable t) => (a -> m) -> t a -> m
-- traverse :: (a -> f b) -> t a -> f (t b)
{-

newtype Const a b = Const { getConst :: a }
instance Functor (Const a) where
  fmap _ (Const x) = Const x
instance Monoid a => Applicative (Const a) where
  pure _ = Const mempty
  -- Const a1 :: Const a (b -> c)
  -- Const a2 :: Const a b
  -- Const $ a1 `mappend` a2 :: Const a c
  -- where b, c are both phantom types
  Const a1 <*> Const a2 = Const $ a1 `mappend` a2

-}

-- (<*>) between (Const) simply accumulates the first monoidal component
foldMap' step = getConst . traverse (Const . step)
                -- where step :: a -> m
                --       Const . step :: a -> Const m b
                --       traverse :: (a -> Const m b) -> t a -> Const m (t b)

