{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE KindSignatures #-}

-- https://en.wikibooks.org/wiki/Haskell/Understanding_arrows

{- 
Just as we think of a monadic type m a as representing a 'computation
delivering an a '; so we think of an arrow type a b c, (that is, the
application of the parameterised type a to the two parameters b and c)
as representing 'a computation with input of type b delivering a c';
arrows make the dependence on input explicit.

—John Hughes, Generalising Monads to Arrows
-}

module UnderstandingArrows where

import Prelude hiding (id, (.))


-- * Pocket guide to 'Arrow'


-- ** Arrows look a lot like functions

class Category (k :: * -> * -> *) where
  id  :: a `k` a
  (.) :: b `k` c -> a `k` b -> a `k` c

  (>>>) :: (a `k` b) -> (b `k` c) -> (a `k` c)
  f >>> g = g . f


-- ** 'Arrow' glides between 'Applicative' and 'Monad'

-- static morphisms
-- Applicative f => f (a `k` b)
-- (<*>) :: Applicative f => f (a `k` b) -> (f a `k` f b)
-- (f a `k` f b) is an Arrow.

-- Kleisli morphisms
-- Monad m => a `k` m b
-- (=<<) :: Monad m => (a `k` m b) -> (m a `k` m b)
-- (m a `k` m b) is an Arrow.

-- Arrows make it possible
-- + to fine tune combination of effects


-- ** An 'Arrow' can multitask

class Category k => Arrow k where
  -- Minimal implementation: arr, first
  arr   :: (a -> b) -> a `k` b
  first :: a `k` b -> (a, z) `k` (b, z)

  second :: a `k` b -> (z, a) `k` (z, b)
  second f = arr swap >>> first f >>> arr swap
    where
      swap (x, y) = (y, x)

  -- Two possible implementations
  (***) :: a `k` c -> b `k` d -> (a, b) `k` (c, d)
  f *** g = first f >>> second g
  -- f *** g = first f . second g

  (&&&) :: a `k` b -> a `k` c -> a `k` (b, c)
  f &&& g = arr twice >>> (f *** g)
    where
      twice x = (x, x)
      -- arr twice              :: a `k` (a, a)
      -- first f                :: (a, z) `k` (b, z)
      -- second g               :: (w, a) `k` (w, c)

-- data Arrow' a b = a :~> b

instance Category (->) where
  id a = a
  (f . g) x = f (g x)

instance Arrow (->) where
  arr = id
  first f = \(a, c) -> (f a, c)


-- ** An 'ArrowChoice' can be resolute

-- Arrow is combining values with Product-type (,)
-- ArrowChoice with Sum-type Either
class Arrow k => ArrowChoice k where
  -- Minimal implementation: left
  left  :: a `k` b -> Either a z `k` Either b z

  -- 'right' w.r.t 'left' similar to 'second' w.r.t. 'first'
  right :: a `k` b -> Either w a `k` Either w b
  right g = arr rev >>> left g >>> arr rev
    where
      rev (Left a) = Right a
      rev (Right b) = Left b

  -- Similar to (***)
  (+++) :: a `k` c -> b `k` d -> Either a b `k` Either c d
  f +++ g = left f >>> right g
            -- where
            --   left f  :: Either a z `k` Either c z
            --   right g :: Either w b `k` Either w d

  -- Similar to (&&&), but swapped
  (|||) :: a `k` c -> b `k` c -> Either a b `k` c
  f ||| g = f +++ g >>> arr fromEither1
    where
      fromEither1 :: Either a a -> a
      fromEither1 (Left  a) = a
      fromEither1 (Right a) = a


-- ** An 'ArrowApply' is just boring

class Arrow k => ArrowApply k where
  -- Applies the first component to the second
  app :: (a `k` b, a) `k` b

instance ArrowApply (->) where
  -- app :: (a -> b, a) -> b
  app = uncurry ($)

-- Chained 'app's can have accumulated states, just like Monad.

{- 
The real flexibility with arrows comes with the ones that aren't
monads, otherwise it's just a clunkier syntax.

—Philippa Cowderoy
-}


-- ** Arrow combinators crop up in unexpected places

{- With Arrow `k` specialized to (->)

(>>>) :: a `k` b -> b `k` c -> a `k` c
(>>>) == flip (.)

first :: a `k` b -> (a, z) `k` (b, z)
first == \f (a, z) -> (f a, z)
first == Data.Bifunctor.first

second :: a `k` b -> (w, a) `k` (w, b)
second == \f (w, a) -> (w, f a)
second == fmap :: (a -> b) -> (w,) a -> (w,) b
second == Data.Bifunctor.second

(***) :: a `k` c -> b `k` d -> (a, b) `k` (c, d)
(***) == \f g -> \(a, b) -> (f a, g b)
(***) == Data.Bifunctor.bimap

(&&&) :: a `k` b -> a `k` c -> a `k` (b, c)
(&&&) == \f g -> \a -> (f a, g a)
(&&&) == liftA2 (,) -- where (Functor (a ->))

with (|) = Either

left :: a `k` b -> (a | z) `k` (b | z)
left == Data.Bifunctor.first

right :: a `k` b -> (w | a) `k` (w | b)
right == fmap :: (a -> b) -> Either w a -> Either w b
right == Data.Bifunctor.second

(+++) :: a `k` c -> b `k` d -> (a | b) `k` (c | d)
(+++) == bimap

(|||) :: a `k` c -> b `k` c -> (a | b) `k` c
(|||) == either :: (a -> c) -> (b -> c) -> (Either a b) -> c

app :: ((a -> b), a) -> b
app == \(f, x) -> f x
app == uncurry ($)
-}

liftF :: (a -> b) -> (r -> a) -> (r -> b)
-- liftF h g = \r -> h (g r)
liftF h g = h . g

liftY :: Arrow y => (a -> b) -> (r `y` a) -> (r `y` b)
liftY h r = r >>> arr h

liftY2 :: Arrow k =>
  (a -> b -> c) -> (r `k` a -> r `k` b -> r `k` c)
liftY2 f2 ra rb = arr (uncurry f2) . (ra &&& rb)                       
                  -- where
                  --   ra &&& rb        :: r `k` (a, b)
                  --   uncurry f2       :: (a, b) -> c
                  --   arr (uncurry f2) :: (a, b) `k` c
