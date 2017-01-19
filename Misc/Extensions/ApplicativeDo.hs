{-# LANGUAGE ApplicativeDo #-}

data Foo a = Foo a deriving (Show)

instance Functor Foo where
  fmap f (Foo a) = Foo (f a)

instance Applicative Foo where
  pure = Foo
  (Foo f) <*> x = fmap f x

f :: Int -> Int -> Int -> Int
f x y z = x*y + y*z + z*x

r1 = f <$> Foo 1 <*> Foo 2 <*> Foo 3

-- prog = print r1

r2 ::
r2 = do
  x <- Foo 1
  y <- Foo 2
  z <- Foo 3
  return $ f x y z


main = print r2

