{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}
{-

https://hackage.haskell.org/package/lens-tutorial-1.0.2/docs/Control-Lens-Tutorial.html

-}

import Control.Lens hiding (element)
-- import Control.Monad.Reader.Class (MonadReader)

-- * Motivation

data Atom = Atom { _element :: String, _point :: Point } deriving (Show)
data Point = Point { _x :: Double, _y :: Double } deriving (Show)

makeLenses ''Atom
makeLenses ''Point


-- With old style:
shiftAtomX_ :: Atom -> Atom
shiftAtomX_ (Atom e (Point x y)) = Atom e $ Point (x + 1.0) y

-- With lens style:
shiftAtomX :: Atom -> Atom
shiftAtomX = over (point . x) (+ 1.0)

p1 = Point {_x = 2.0, _y = 2.0}
a1 = Atom {_element = "C", _point = p1}


data Molecule = Molecule { _atoms :: [Atom] } deriving (Show)

makeLenses ''Molecule

shiftMoleculeX :: Molecule -> Molecule
shiftMoleculeX = over (atoms . traverse . point . x) (+ 1.0)

m1 = Molecule { _atoms = [
                  a1,
                  Atom {_element = "D", _point = Point { _x = 3.0, _y = 4.0 }}]}

-- * Lenses

data Lens_ a b = Lens_
  { view_ :: a -> b
  , over_ :: (b -> b) -> (a -> a)
  }

-- Lens' <whole> <part>

type Lens_' a b = forall f . Functor f => (b -> f b) -> (a -> f a)
-- with (f = Identity)
type ASetter_' s a = (a -> Identity a) -> (s -> Identity s)
-- ~= (a -> a) -> (s -> s)
-- for ‘over’
type Getting_' r s a = (a -> (Const r) a) -> (s -> (Const r) s)
-- ~= (a -> a) -> (s -> r)
-- ~=             (s -> r)
-- for ‘view’

-- Unifying the operation of setting and getting into one class.


-- Can manually make lenses with ‘lens’ smart constructor.

-- lens
--   :: Functor f => (s -> a) -> (s -> b -> t) -> (a -> f b) -> s -> f t
--   == Functor f => (s -> a) -> (s -> b -> t) -> Lens s t a b

-- type Lens s t a b =
--   forall (f :: * -> *). Functor f => (a -> f b) -> s -> f t

-- type Lens' s a = Lens s s a a ==
--   forall (f :: * -> *). Functor f => (a -> f a) -> s -> f s


-- With specialized version where (s == t) and (a == b)
-- lens
--   :: Functor f => (s -> a) -> (s -> a -> s)
--                   -> Lens' s a
-- i.e.
--   :: Functor f => (s -> a) -> (s -> a -> s)
--                   ->
--                   (a -> f a) -> (s -> f s)
--
-- With meaning:
-- lens
--   :: Functor f => (getter) -> (setter) -> (lens-instance)

point_ :: Lens' Atom Point
point_ = lens _point (\atm newPnt -> atm { _point = newPnt })

-- pointF :: Lens' Atom Point
pointF :: Functor f => (Point -> f Point) -> Atom -> f Atom
pointF k atm = fmap (\newPnt -> atm { _point = newPnt }) $ k (_point atm)
  -- where
  --   the update function (\a -> s { _a = a } ) :: (a -> s)
  --   is lifted by fmap, i.e. (fmap (\a -> ...)) :: f a -> f s


-- ** Combinating lenses with function composition

-- Specializing
-- (.) :: (b -> c) -> (a -> b) -> (a -> c)
-- 
-- to
-- (.) :: Lens' τ s -> Lens' s a -> Lens' τ a
-- 
-- which is indeed
-- (.) :: Functor f =>
--      ((s -> f s) -> (τ -> f τ))
--   -> ((a -> f a) -> (s -> f s))
--   -> ((a -> f a) -> (τ -> f τ))

-- e.g.
-- 
-- With
-- 
-- point :: Lens' Atom Point
-- x     :: Lens' Point Double
-- 
-- then
-- 
-- point . x :: Lens' Atom Double


-- ** Using lenses

-- With simplified versions
-- 
-- view :: Lens' s a -> (s -> a)
-- over :: Lens' s a -> (a -> a) -> (s -> s)
-- set  :: Lens' s a ->        a -> (s -> s)
--   where
--     set ls a ss = over ls (\_ -> a) ss

-- Distribution laws
-- 
-- view (lens1 . lens2) = (view lens2) . (view lens1)
-- view id = id
--
-- over (lens1 . lens2) = (over lens1) . (over lens2)
-- over id = id



-- * Accessor notation

-- (^.) :: s -> Lens' s a -> a
-- s ^. lns -> view lns s

d = a1 ^. point . x



-- * First-class

-- Given
-- 
-- shiftAtomX     :: Atom -> Atom
-- shiftMoleculeX :: Molecule -> Molecule


-- Unify them
-- shift :: ASetter s s Double Double -> s -> s
shift :: ASetter' s Double -> s -> s
shift lns = over lns (+ 1.0)
  -- spec
  --   over :: ASetter s s Double Double -> (Double -> Double) -> s -> s

shiftAtomX' = shift (point . x)
shiftMoleculeX' = shift (atoms . traverse . point . x)


-- Can use synonyms for composite lenses
-- ! Ignoring signature may lead to ambiguity.
atomX :: Lens' Atom Double
atomX = point . x

-- moleculeX :: Lens' Molecule Double
-- moleculeX = atoms . traverse . point . x


-- * Traversals
-- + A first class getter and setter for an arbitrary number of values
{-
data Traversal_' a b = Traversal_'
  { toListOf_' :: a -> [b]
  , over_'     :: (b -> b) -> (a -> a)
  }
-}

-- moleculeX :: Traversal' Molecule Double

-- Traversal' is for getting/setting *zero or more* values.
-- Lens'      is for getting/setting exactly one value.

type Traversal'' s a =
  forall f . Applicative f => (a -> f a) -> (s -> f s)

type Lens'' s a =
  forall f . Functor f     => (a -> f a) -> (s -> f s)

-- (Lens') is specialized (Traversal') since constraint (Functor) is
-- more general than (Applicative).

{- Can have more general getter/setter utility using Traversal'
atoms   :: Traversal' Molecule [Atom]
element :: Traversal' Atom     String
point   :: Traversal' Atom     Point
x       :: Traversal' Point    Double
y       :: Traversal' Point    Double
-}


-- From the (Traversable) context,
--
-- traverse :: (Applicative f, Traversable t) =>
--          (a -> f b) -> t a -> f (t b)
--       spec (b = a)
--          (a -> f a) -> t a -> f (t a)
--          (a -> f a) ->   s -> f s
--       ~= Lens' s a
-- 

-- ** 3 ways to make traversals
-- + traverse is Traversal'
-- + Lens' can type-check as Traversal'
-- + Prism' is also Traversal'


-- ** Combination of traversals

-- (.) :: Traversal' τ s -> Traversal' s a -> Traversal' τ a
-- (.) :: Applicative f =>
--        ((s -> f s) -> (τ -> f τ))
--     -> ((a -> f a) -> (s -> f s))
--     -> ((a -> f a) -> (τ -> f τ))

-- atoms                    :: Traversal' Molecule [Atom]
--                          == ([Atom] -> f [Atom]) -> Molecule -> f Molecule
--
-- atoms . traverse         :: Traversal' Molecule Atom
--   with traverse :: (a -> f b) -> t a -> f (t b)
--        spec
--          t == []
--          a == b == Atom
--        where
--          traverse :: Traversal' [Atom] Atom
--                   == (Atom -> f Atom) -> [Atom] -> f [Atom]
-- 
-- atoms . traverse . point :: Traversal' Molecule Point
--   with (atoms . traverse) :: (Atom -> f Atom) -> (Molecule -> f Molecule)
--        point              :: (Point -> f Point) -> (Atom -> f Atom)
--   == (Point -> f Point) -> (Molecule -> f Molecule)

travAtoms :: Traversal' [Atom] Atom
travAtoms = traverse

wawas = traverse 
  (\atm -> Just (atm {_element = "wawa"}))
  [Atom {_element = "a", _point = p1},
   Atom {_element = "b", _point = p1 {_y = 12.3}}]


shiftMoleculeY :: Double -> Molecule -> Molecule
shiftMoleculeY d = over (atoms . traverse . point . y) (+ d)
  -- where
  --   (atoms . traverse . point . y) :: (Double -> f Double) -> Molecule -> f Molecule
  --   over :: ASetter s t a b -> (a -> b) -> s -> t
  --        == ((a -> Identity b) -> (s -> Identity t))
  --        -> (a -> b)
  --        -> (s -> t)
  --   spec
  --     a = b = Double
  --     s = t = Molecule
  --     f = Identity
  --     over :: ((Double -> Identity Double) -> (Molecule -> Identity Molecule))
  --          -> (Double -> Double)
  --          -> (Molecule -> Molecule)

over1 :: ((a -> Identity b) -> (s -> Identity t)) -> (a -> b) -> (s -> t)
over1 lns a2b = let a2Ib = \a -> Identity $ a2b a
                    s2It = lns a2Ib
                in  runIdentity . s2It

(#.) :: Functor f => (a -> f b) -> (b -> c) -> a -> f c
ctor #. f = fmap f . ctor

over2 :: ((a -> Identity b) -> (s -> Identity t)) -> (a -> b) -> (s -> t)
over2 l f = runIdentity . l (Identity #. f)



-- * Types

over3 ::
  ((a -> Identity b) -> (s -> Identity t)) -> (a -> b) -> (s -> t)
over3 setter f s = runIdentity $ setter (\a -> Identity (f a)) s

-- type Getting r s a = (a -> Const r a) -> s -> Const r s
-- view :: MonadReader s m => (Getting a s a) -> m a
view3 ::
  ((a -> Const a a) -> (s -> Const a s)) -> s -> a
view3 getter s = getConst $ getter (\a -> Const a) s

-- (Const) implements (Applicative) as
-- instance Monoid b => Applicative (Const b)

es = view3 (atoms . traverse . element) m1


-- The result of (Traversal') needs to be (Monoid) when applied to (view).



-- * Drawbacks
-- 
-- • Error messages only understandable after Traversal'
--
-- • Learning curve
-- 
-- • Auto-generating lenses increase compile times
-- 
-- • Large dependency tree
--   + lightweight alternatives:
--     + lens-simple
--     + microlens
