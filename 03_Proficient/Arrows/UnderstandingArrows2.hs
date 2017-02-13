{-# LANGUAGE TypeOperators #-}
module UnderstandingArrows2 where

import Control.Applicative
import Control.Monad.Trans.State

import Prelude hiding (id, (.))


class Category y where
  -- Minimal implementation
  id  :: a `y` a
  (.) :: b `y` c -> a `y` b -> a `y` c
  
  -- Dependent utilities
  (>>>) :: a `y` b -> b `y` c -> a `y` c
  (>>>) = flip (.)

class Category y => Arrow y where
  -- Minimal implementation
  arr   :: (a -> b) -> a `y` b
  first :: a `y` b -> (a, z) `y` (b, z)

  -- Dependent utilities
  second :: a `y` b -> (w, a) `y` (w, b)
  second r = arr swap >>> first r >>> arr swap
    where
      swap (p, q) = (q, p)

  (&&&) :: a `y` b -> a `y` c -> a `y` (b, c)
  f &&& g = arr twice >>> first f >>> second g
    where
      twice x = (x, x)
    -- where
    --   f       :: a `y` b
    --   first f :: (a, z) `y` (b, z)
    --   g        :: a `y` c
    --   second g :: (w, a) `y` (w, c)
    --   g >>> first f ::

  (***) :: a `y` c -> b `y` d -> (a, b) `y` (c, d)
  f *** g = first f >>> second g
    -- where
    --   first f  :: (a, z) `y` (c, z)
    --   second g :: (w, b) `y` (w, d)

instance Category (->) where
  id x = x
  (f . g) x = f (g x)

instance Arrow (->) where
  -- arr :: (a -> b) -> (a -> b)
  arr = id
  -- first :: (a -> b) -> ((a, z) -> (b, z))
  first f = \(a, z) -> (f a, z)

class Arrow y => ArrowChoice y where
  -- Minimal implementation left
  left :: a `y` b -> (a `Either` z) `y` (b `Either` z)

  right :: a `y` b -> (w `Either` a) `y` (w `Either` b)
  right r = arr eFlip >>> left r >>> arr eFlip
    where eFlip (Left x) = Right x
          eFlip (Right x) = Left x

  (+++) :: a `y` c -> b `y` d -> (a `Either` b) `y` (c `Either` d)
  f +++ g = left f >>> right g

  (|||) :: a `y` c -> b `y` c -> (a `Either` b) `y` c
  f ||| g = left f >>> right g >>> arr fromEither
    where fromEither (Left c) = c
          fromEither (Right c) = c
    -- where
    --   left f  :: (a E z) `y` (c E z)
    --   right g :: (w E b) `y` (w E c)
    --   left f >>> right g ::
    --     (a E b) `y` (c E c)

-- * Using arrows

-- ** Avoiding leaks

-- Motivated by an efficient parser design

type ParserT m a = StateT String m a

runParseT :: ParserT m a -> (String -> m (a, String))
runParseT = runStateT

char :: Char -> ParserT Maybe ()
char c = StateT match
  where match [] = Nothing
        match (s:ss) = if s == c then Just ((), ss) else Nothing
abc = char 'a' <|> char 'b' <|> char 'c'
one = do char 'o' >> char 'n' >> char 'e'
         return "one"
two = do char 't' >> char 'w' >> char 'o'
         return "one"
three = do char 't' >> char 'h' >> char 'r' >> char 'e' >> char 'e'
           return "one"
nums = one <|> two <|> three

