{-# LANGUAGE TemplateHaskell, RankNTypes #-}
module TasteLenses where

-- import Data.Functor.Contravariant
import Control.Lens
import Control.Monad.State


-- * A Taste of Lenses * --


{- conventional stuff
data Point = Point { posX :: Double,
                     posY :: Double
                   } deriving (Show)

data Segment = Segment { segStart :: Point,
                         segEnd :: Point
                       } deriving (Show)

mkPoint (x, y) = Point x y
mkSeg st ed = Segment (mkPoint st) (mkPoint ed)
-}


data Point = Point
  { _posX :: Double, _posY :: Double
  } deriving (Show)
makeLenses ''Point
{- Generated lenses
posX :: Functor f => (Double -> f Double) -> Point -> f Point
posY :: Functor f => (Double -> f Double) -> Point -> f Point
-}


data Segment = Segment
  { _segStart :: Point
  , _segEnd :: Point
  } deriving (Show)
makeLenses ''Segment
{-
segStart :: Functor f => (Point -> f Point) -> Segment -> f Segment
segEnd   :: Functor f => (Point -> f Point) -> Segment -> f Segment
-}

-- ** Lens utils
{-
set :: ASetter s t a b -> b -> s -> t

over :: ASetter s t a b -> (a -> b) -> s -> t

view
  :: Control.Monad.Reader.Class.MonadReader s m =>
     Getting a s a -> m a
-}

{- Instantiation examples

λ> :t set segEnd
set segEnd :: Point -> Segment -> Segment

λ> :t view segEnd
view segEnd
  :: Control.Monad.Reader.Class.MonadReader Segment m => m Point

-}

mkPoint (x, y) = Point x y

mkSeg st ed = Segment (mkPoint st) (mkPoint ed)

  

-- * The Scenic Route to Lenses * --


-- ** Traversals

{- Warmup with Traversals
traverse :: (Applicative f, Traversable t) => (a -> f b) -> (t a -> f (t b))
-}

myFMap f = runIdentity . traverse (Identity . f)
myFoldMap f = getConst . traverse (Const . f)


-- A special setter rather than ASetter
pointCoords :: Applicative f => (Double -> f Double) -> Point -> f Point
pointCoords g (Point x y) = Point <$> g x <*> g y -- Applicative using (g): x and y are of the same type

keepIfPositive :: (Ord x, Num x) => x -> Maybe x
keepIfPositive x | x < 0     = Nothing
                 | otherwise = Just x

type MyTraversal s t a b =
  forall f. Applicative f => (a -> f b) -> s -> f t

-- Ex
extremityCoords :: Applicative f => (Double -> f Double) -> Segment -> f Segment
extremityCoords g (Segment p1 p2) = Segment <$> pointCoords g p1 <*> pointCoords g p2

-- ** Setters

type MySetter s t a b =
  forall f. Settable f => (a -> f b) -> (s -> f t)
  -- Traversal is anyway a special Setter: it applies some Applicative to sub-structures.
  -- Setter is not a special Traversal: it may only apply Applicative to some sub-structures.
  -- ! Setter is more general than Traversal.

{- ASetter definition
λ> :i ASetter
type ASetter s t a b = (a -> Identity b) -> s -> Identity t
  	-- Defined in ‘Control.Lens.Setter’
-}

-- ** `over` is a combinator for setters
-- 
-- set :: ASetter s t a b -> b -> s -> t
--    ::: (ASetter oldObj newObj oldVal newVal) -> newVal -> oldObj -> newObj
--        ! (b) is the plain target value
-- over :: ASetter s t a b -> (a -> b) -> s -> t
--    ::: (ASetter oldObj newObj oldVal newVal) -> (oldVal -> newVal) -> oldObj -> newObj
--         ! (a -> b) is a function used to generate the target value

p1 = Point 1 2
p2 = set posX 7 p1
p3 = over posX (+100) p1
p4 = (over posX) (*3) p1

-- Ex 1.
scalePoint :: Double -> Point -> Point
-- scalePoint fctr = (over posY (*fctr)) . (over posX (*fctr))
scalePoint c = over pointCoords (*c)

scaleSegment :: Double -> Segment -> Segment
scaleSegment c = over extremityCoords (*c)

-- Ex 2.
-- fmap == over mapped
myMapped :: (Functor f) => (a -> Identity b) -> f a -> Identity (f b)
-- cf.
-- traverse :: (Applicative f, Traversable t) => (a -> f b) -> t a -> f (t b)
--
-- type ASetter s t a b = (a -> Identity b) -> s -> Identity t
-- set  :: ASetter s t a b -> b -> s -> t
-- over :: ASetter s t a b -> (a -> b) -> s -> t
-- over :: ASetter (f a) (Identity (f b)) a (Identity b)
myMapped g = Identity . fmap (runIdentity . g)



-- * Folds * --

type Fold s a =
  forall f. (Contravariant f, Applicative f) => (a -> f a) -> s -> f s

{- What is Contravariant?

class Contravariant (f :: * -> *) where
  contramap :: (a -> b) -> f b -> f a
  -- LAWS
  contramap id == id
    where id :: a -> a
          contramap id :: f a -> f a

  contramap (g . f) == contramap f . contramap g
    where f     :: a -> b
          g     :: b -> c
          g . f :: a -> c
          (Applicative h, Contravariant h)
          contramap f               :: h b -> h a
          contramap g               :: h c -> h b
          contramap (g . f)         :: h c -> h a
          contramap f . contramap g :: h c -> h a

lt4 :: Predicate Int = { getPredicate :: Int -> Bool}
length :: String -> Int
contramap length lt4 ::
  contramap :: ([a] -> Int) -> Predicate Int -> Predicate [a]
  contramap length :: Predicate Int -> Predicate [a]
  contramap length lt4 :: Predicate [a]
  getPredicate . contramap length lt4 :: [a] -> Bool

-}

{-
toListOf :: Getting (Data.Monoid.Endo [a]) s a -> s -> [a]

toListOf extremityCoordinates (makeSegment (0, 1) (2, 3))

preview
  :: Control.Monad.Reader.Class.MonadReader s m =>
     Getting (Data.Monoid.First a) s a -> m (Maybe a)
-}


-- * Getters * --

type Getter' s a =
  forall f. (Contravariant f, Functor f) => (a -> f a) -> s -> f s

-- someGetter :: (a -> Const r a) -> s -> Const r s

-- to :: (Contravariant f, Profunctor p) => (s -> a) -> Optic' p f s ab

-- view
--   :: Control.Monad.Reader.Class.MonadReader s m =>
--      Getting a s a -> m a

-- hasn't :: Getting All s a -> s -> Bool


-- * Lenses at last * --

-- type Traversal s t a b =
--   forall f. Applicative f => (a -> f b) -> s -> f t
-- 
-- type Lens s t a b =
--   forall f. Functor f => (a -> f b) -> s -> f t
--
-- -- Relaxing (Applicative) of Traversal to (Functor) getting Lens!
-- 
-- 

-- _1 (\x -> [0..x]) (4, 1)
-- set _1 7 (4, 1)
-- over _1 length ("orange", 1)
-- toListOf _1 (4, 1)
-- view _1 (4, 1)

-- ** Composition


-- ** Operators


-- ** State manipulation
stateExample :: State Segment ()
stateExample = do
  segStart .= mkPoint (0, 0)
  zoom segEnd $ do
    posX += 1
    posY *= 2
    pointCoords %= negate

-- ** Isos

unmkPoint :: Point -> (Double, Double)
unmkPoint (Point x y) = (x, y)

-- iso :: (s -> a) -> (b -> t) -> Iso s t a b
pointPair :: Iso' Point (Double, Double)
pointPair = iso unmkPoint mkPoint

-- view pointPair testPoint


-- ** Prisms
{-
Iso         --> one tar, invertible
Lens        --> one tar, not invertible
Traversable --> many tar, not invertible
-}

-- Prism is a Traversal
-- set _Just 5 (Just "orange")

-- Prism is not a Getter


-- Ex 2. Implement (lens)
myLens :: Functor f => (s -> a) -> (s -> b -> t) -> (a -> f b) -> s -> f t
myLens getter setter k s = setter s <$> k (getter s)
  -- where
  --   a --> attr
  --   b --> attrNew
  --   s --> struct
  --   t --> structNew
  --   
  --   getter :: s -> a
  --   setter :: s -> b -> t
  --   k      :: a -> f b
  --   s      :: s
  -- 
  --   setter s :: b -> t
  --   getter s :: a
  -- 
  --   (setter s <$>)          :: f (b -> t)
  --   k (getter s)            :: f b
  --   (setter s <$> getter s) :: f (b -> t) <$> f b == f t


-- Feeling failed and dizzy with interveaning concepts...


main :: IO ()
main = putStrLn $ show $
  execState stateExample (mkSeg (1,2) (5,3))

