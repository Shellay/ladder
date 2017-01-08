-- * Expr with two types: Int and Bool * --

{-
data Expr = I Int
          | Add Expr Expr
          | Mul Expr Expr
          | B Bool
          | Eq Expr

-- Hard expandable

eval :: Expr -> Maybe (Either Int Bool)
eval (I n) = Left n
eval (B b) = Right b
eval (Add e1 e2) = ...

-- Expressino of non-sense typechecks!
(B True) `Add` (I 5) :: Expr

-}


data Expr a = I Int
            | B Bool
            | Add (Expr a) (Expr a) -- Add :: Expr a -> Expr a -> Expr a
            | Mul (Expr a) (Expr a)
            | Eq (Expr a) (Expr a)

-- * smart constructor
add :: Expr Int -> Expr Int -> Expr Int
add = Add

i = (I :: Int -> Expr Int)
b = (B :: Bool -> Expr Bool)

-- Does not typecheck! Yeah!
-- r = (b True) `add` (i 5)

-- But the evaluation
eval :: Expr a -> a
eval (I n) = n
  -- where type (a) is irrelevant to (Int),
  -- which is not as intended

-- and this is valid...
r = I 5 :: Expr String

{- * How can a constructor return a specialized type?
---- GADTs
-}

main = return ()
