{-
A short introduction by Andres Löh @ well-typed.com

https://www.andres-loeh.de/HaskellForDSLs.pdf

-- many helpful links and resources
-}

-- * Shallow embedding
-- | e.g. Lens
-- | e.g. Parsec


-- * Deep embedding
-- | e.g. Accelerate


-- * E.g. 
{-
(⊕) :: Expr -> Expr -> Expr
one :: Expr
eval :: Expr -> Int
-}

-- ** Shallow
{-
type Expr = Int
(⊕) = (+)
one = 1
eval = id

tree :: Int -> Expr
tree 0 = one
tree n = let shared = tree (n-1)
         in shared ⊕ shared     -- good using of sharing
-}

-- ** Deep

{-
(⊕) :: Expr -> Expr -> Expr
one :: Expr
eval :: Expr -> Int

data Expr = PI Expr Expr | One

(⊕) = PI
one = One
eval (PI e₁ e₂) = eval e₁ + eval e₂
eval One = 1

disp :: Expr -> String
disp (PI e₁ e₂) = "(" ++ disp e₁ ++ " + " ++ disp e₂ ++ ")"
disp One = "1"


tree :: Int -> Expr
tree 0 = one
tree n = let shared = tree (n-1)
         in shared ⊕ shared     -- sharing unavailable! bad for (tree 100)
-}

-- solution: parametric higher-order abstract syntax

data Expr a = PI (Expr a) (Expr a)
            | One
            | Var a
            | Let (Expr a) (a -> Expr a) -- use function for sharing

(⊕)  :: Expr a -> Expr a -> Expr a
(⊕) = PI

one  :: Expr a
one = One

tree :: Int -> Expr a
tree 0 = one
tree n = Let (tree (n-1)) (\shared -> (Var shared) ⊕ (Var shared))

eval :: Expr Int -> Int
eval (PI e₁ e₂) = eval e₁ + eval e₂
eval One = 1
eval (Var x) = x
eval (Let e f) = eval $ f (eval e)
