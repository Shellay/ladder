{-# LANGUAGE GADTs, RankNTypes, KindSignatures #-}

-- * Constructors can specialize data * --

data Expr a where
  I :: Int -> Expr Int
  B :: Bool -> Expr Bool
  Add :: Expr Int -> Expr Int -> Expr Int
  Mul :: Expr Int -> Expr Int -> Expr Int
  Eq :: Expr Int -> Expr Int -> Expr Bool

-- These typecheck! 
eval :: Expr a -> a
eval (I n) = n
eval (B b) = b
eval (Add e1 e2) = eval e1 + eval e2
eval (Mul e1 e2) = eval e1 * eval e2
eval (Eq  e1 e2) = eval e1 == eval e2


data RoseTree a
  where RoseTree :: a -> [RoseTree a] -> RoseTree a


data FooInGadtClothing a where
  MkFooInGadtClothing :: a -> FooInGadtClothing a

-- Eqv in Haskell98
data Haskell98Foo a = MkHaskell98Foo a

-- No eqv in Haskell98
data TrueGadtFoo a where
  MkTrueGadtFoo :: a -> TrueGadtFoo Int



-- * Safe list, where safeHead never accepts Nil at runtime. * --

data Empty
data NonEmpty

data SafeList a eOrNe where
  Nil  :: SafeList a Empty
  Cons :: a -> SafeList a eOrNe -> SafeList a NonEmpty

safeHead :: SafeList a NonEmpty -> a
safeHead (Cons x _) = x

-- Write reporting by type-checking!
-- r1 = safeHead (Cons "hi" Nil)
-- r2 = safeHead Nil               -- • Couldn't match type ‘Empty’ with ‘NonEmpty’

-- But trouble - this function cannot be defined!
-- -- Cannot return a consistent type...
-- silly :: Bool -> SafeList a ?
-- silly False = Nil
-- silly True = Cons () Nil


-- ** Allow a function to make both empty and non-empty list (non-safe)

-- The 2 marks
data NotSafe
data Safe

data MarkedList :: * -> * -> * where
  MNil          :: MarkedList t NotSafe
  MCons         :: a -> MarkedList a m1 -> MarkedList a m2

mSafeHead :: MarkedList a Safe -> a
mSafeHead (MCons x _) = x

-- Definable... but 'NotSafe' should not be used for typing the second branch.
mSilly :: Bool -> MarkedList () NotSafe
mSilly False = MNil
mSilly True = MCons () MNil

-- More trouble... Tail function not definable!
-- mSafeTail :: MarkedList a Safe -> MarkedList a ???
-- mSafeTail (MCons _ xs) = xs


-- ** To support safeTail!
data RecSafe b

data MSList :: * -> * -> * where
  MSNil     :: MSList t NotSafe
  MSCons    :: a -> MSList a b -> MSList a (RecSafe b) -- Huge trick! Think of Free-Monad!

msSafeHead :: MSList a (RecSafe b) -> a
msSafeHead (MSCons a _) = a

msSafeTail :: MSList a (RecSafe b) -> (MSList a b)
msSafeTail (MSCons _ as) = as


-- * Type with values - a list with length value at type-level! * --
data Zero
data Succ n

data LenList :: * -> * -> * where
  LenNil     :: LenList any Zero
  LenCons    :: a -> LenList a n -> LenList a (Succ n)

lenSafeHead :: LenList a (Succ n) -> a
lenSafeHead (LenCons a _) = a

lenSafeTail :: LenList a (Succ n) -> LenList a n
lenSafeTail (LenCons _ as) = as


main :: IO ()
main = return ()
