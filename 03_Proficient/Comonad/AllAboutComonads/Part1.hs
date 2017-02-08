-- http://comonad.com/haskell/Comonads_1.pdf
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators #-}

module Part1 where

import Prelude hiding (id, (.))

-- * Categories

-- Category
-- + objects, arrows
-- + ID
-- + ASSOC composition


-- Hask = Cat(object=type, arrow=(type -> type))

  
-- * Categories in Haskell

class Category (k) where
  id  :: a `k` a
  (.) :: (b `k` c) -> (a `k` b) -> (a `k` c)

instance Category (->) where
  id x = x
  (f . g) x = f (g x)


newtype Dual k a b = Dual (k b a)

instance Category k => Category (Dual k) where
  id = Dual id
  Dual f . Dual g = Dual (g . f)



-- * Functors

class Functor' f where
  fmap' :: Category k => (a `k` b) -> (f a `k` f b)

-- Cofunctor == Functor
class Cofunctor f where
  cofmap :: Category k => (b `k` a) -> (f b `k` f a)

class ContravariantFunctor f where
  contrafmap :: Category k => (b `k` a) -> (f a `k` f b)
-- | e.g. contra
{-
newtype Test a = Test { runTest :: a -> Bool }
instance ContravariantFunctor Test where
  contrafmap f (Test g) = Test (g . f)
isZero = Test (==0)
isEmpty = contrafmap length isZero
-}

-- | Just for convenience of reverse-ordering the composition.
-- | c.f. arrows, comonads


-- ** Functors in Haskell Redux
-- |
-- | only covariant Hask endofunctors

-- ** Functor in category-extras
class (Category p, Category q) =>
  Functor'' f p q | f p -> q, f q -> p where
  fmap'' :: (a `p` b) -> (f a `q` f b)

class (Category (Dom f), Category (Cod f)) => Functor''' f where
  type Dom f :: * -> * -> *
  type Cod f :: * -> * -> *
  fmap''' :: Dom f a b -> Cod f (f a) (f b)



-- * Monads

-- class Category k => Monad' m k where
--   unit :: a `k` m a
--   bind :: (a `k` m b) -> (m a `k` m b)

class Functor' m => Monad' m where
  unit :: Category k => a `k` m a
  bind :: Category k => (a `k` m b) -> (m a `k` m b)

  join :: Category k => m (m a) `k` m a
  join = (bind id)
  -- where
  --   id   :: m a `k` m a
  --   bind :: m a `k` m a -> m (m a) `k` m a

  bind' :: Category k => (a `k` m b) -> (m a `k` m b)
  bind' f = join . fmap' f
  -- where
  --   fmap' :: (a `k` b) -> (m a `k` m b)


-- * Comonads

-- class Functor' m => Comonad m where
--   counit :: Category k => m a `k` a
--   cobind :: Category k => (m b `k` a) -> (m b `k` m a)

-- (Functor == Cofunctor) while (Monad != Comonad)

class Functor' w => Comonad w where
  extract :: Category k => w a `k` a
  extend  :: Category k => (w a `k` b) -> (w a `k` w b) 

  -- | Laws
  -- extend extract = id
  -- extract . extend f = f
  -- extend f . extend g = extend (f . extend g)

  duplicate :: Category k => w a `k` w (w a)
  duplicate = extend id

  extend' :: Category k => (w a `k` b) -> (w a `k` w b)
  extend' h = fmap' h . duplicate
  -- where
  --   h         :: w a `k` b
  --   duplicate :: w a `k` w (w a)
  --   fmap' h   :: w (w a) `k` w b
  --   (.)       :: w (w a) `k`    w b
  --             ->    w a  `k` w (w a)
  --             -> w a `k` w b

  -- | Here (fmap) does UNLIFTING
  -- | e.g.
  -- |
  -- | fmap (\Just x -> x) (Just (Just 9)) === Just 9
  -- |
  -- | fmap (m x `k` x) :: (m (m y)) -> m y
