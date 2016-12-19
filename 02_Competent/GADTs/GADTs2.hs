{-# LANGUAGE GADTs, RankNTypes, KindSignatures #-}

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

main :: IO ()
main = return ()


data RoseTree a
  where RoseTree :: a -> [RoseTree a] -> RoseTree a


data FooInGadtClothing a where
  MkFooInGadtClothing :: a -> FooInGadtClothing a

-- Eqv in Haskell98
data Haskell98Foo a = MkHaskell98Foo a

-- No eqv in Haskell98
data TrueGadtFoo a where
  MkTrueGadtFoo :: a -> TrueGadtFoo Int



{- * Safe list, where safeHead never accepts Nil at runtime. -}

data Empty
data NonEmpty

data SafeList a eOrNe where
  Nil  :: SafeList a Empty
  Cons :: a -> SafeList a eOrNe -> SafeList a NonEmpty

safeHead :: SafeList a NonEmpty -> a
safeHead (Cons x _) = x

-- r1 = safeHead (Cons "hi" Nil)
-- r2 = safeHead Nil               -- • Couldn't match type ‘Empty’ with ‘NonEmpty’

-- A trouble
-- silly :: Bool -> SafeList a ?
-- silly False = Nil
-- silly True = Cons () Nil


{- * Allow a function to make both empty and non-empty list (non-safe) -}

-- The 2 marks
data NotSafe
data Safe

data MarkedList :: * -> * -> * where
  MNil          :: MarkedList t NotSafe
  -- MCons must be safe?
  -- MCons         :: a -> MarkedList a m1 -> MarkedList a Safe
  MCons         :: a -> MarkedList a m1 -> MarkedList a m2

mSafeHead :: MarkedList a Safe -> a
mSafeHead (MCons x _) = x

-- mSafeTail :: MarkedList a Safe -> MarkedList a ???
-- mSafeTail (MCons _ xs) = xs

mSilly :: Bool -> MarkedList () NotSafe
mSilly False = MNil
mSilly True = MCons () MNil


{- * To support safeTail! -}
data RecSafe b

data MSList :: * -> * -> * where
  MSNil     :: MSList t NotSafe
  MSCons    :: a -> MSList a b -> MSList a (RecSafe b)

msSafeHead :: MSList a (RecSafe b) -> a
msSafeHead (MSCons a _) = a

msSafeTail :: MSList a (RecSafe b) -> (MSList a b)
msSafeTail (MSCons _ as) = as


{- * Type with values - a list with length value! -}
data Zero
data Succ n

data LenList :: * -> * -> * where
  LenNil     :: LenList any Zero
  LenCons    :: a -> LenList a n -> LenList a (Succ n)

lenSafeHead :: LenList a (Succ n) -> a
lenSafeHead (LenCons a _) = a

lenSafeTail :: LenList a (Succ n) -> LenList a n
lenSafeTail (LenCons _ as) = as

