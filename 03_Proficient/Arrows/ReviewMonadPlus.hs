{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ConstrainedClassMethods #-}
{-# LANGUAGE RankNTypes #-}

module ReviewMonadPlus where

import Data.Char (ord, chr)
import Control.Monad.Trans.State

class Applicative f => Alternative f where
  empty :: f a
  (<|>) :: f a -> f a -> f a

instance Alternative Maybe where
  empty = Nothing
  Nothing <|> m = m
  m <|> _ = m

instance Alternative [] where
  empty = []
  (<|>) = (++)

instance Monoid a => Alternative (Either a) where
  empty = Left mempty
  Left a  <|> r       = r
  Right b <|> _       = Right b

instance Alternative (Either a) => MonadPlus (Either a)

class (Alternative m, Monad m) => MonadPlus m where
  mzero :: MonadPlus m => m a
  mzero = empty
  mplus :: MonadPlus m => m a -> m a -> m a
  mplus = (<|>)

instance (Monad m, Alternative m) => Alternative (StateT s m) where
  empty = StateT $ \s -> empty
  st1 <|> st2 = StateT $ \s -> 
    runStateT st1 s <|> runStateT st2 s

instance (MonadPlus m) => MonadPlus (StateT s m) where
  mzero = empty
  mplus = (<|>)

-- * Example: parallel parsing

-- type StateT s m a = s -> m (a, s)
-- type Parser m a = ([Char] -> m (a, [Char]))
type Parser m a = StateT String m a

parser :: (String -> m (a, String)) -> StateT String m a
parser = StateT

runParser :: Parser m a -> (String -> m (a, String))
runParser = runStateT


digit :: (MonadPlus m) => Int -> Parser m Int
digit i = StateT parse
  where
    parse (c:cs) = if [c] == show i
                   then return (i, cs)
                   else empty
    parse _ = empty

binChar :: (MonadPlus m) => Parser m Int
-- binChar = StateT $ \s ->
--   runStateT (digit 0) s <|> runStateT (digit 1) s
binChar = digit 0 <|> digit 1


-- ** MonadPlus

-- class Monad m => MonadPlus m where
--   mzero :: m a
--   mplus :: m a -> m a -> m a


-- ** More parsers
 
-- parser :: StateT String (Either String) a
-- parser = StateT

uEnd :: Either String a
uEnd = Left "Unexpected end."

whitespace :: Parser (Either String) ()
whitespace = StateT f
  where f []       = Left "Unexpected end."
        f (' ':cs) = Right ((), cs)
        f _        = Left "Expecting whitespace char."

oneOf :: [Parser (Either String) a] -> Parser (Either String) a
oneOf = foldr (<|>) (parser $ \s -> Left "Expecting digit.")

many :: Parser (Either String) a -> Parser (Either String) [a]
many p = StateT $ \s ->
  case runParser p s of
    Left _ ->
      Right ([], s)
    Right (a, s1) ->
      do (as, s2) <- runParser (many p) s1
         Right (a:as, s2)

-- digits :: 
anyDigit = oneOf $ (map digit [0..9])

seqDigit = many anyDigit

readInt  = fmap (fst . carry) seqDigit
  where
    carry = foldr (\a (n, cry) -> (a * cry + n, cry * 10)) (0, 1)
