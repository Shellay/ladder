{-# LANGUAGE RankNTypes #-}
module HigherKindedLearn where

{- ** Warmup -}

-- Simulate type classes with Data with rank-2 type
-- Internal implementation of type classes!

data MyMonad m = Mon { unit :: forall t. t -> m t,
                       bind :: forall t1 t2. m t1 -> (t1 -> m t2) -> m t2 }

myMapM :: MyMonad m -> (a -> m b) -> [a] -> m [b]
myMapM m@(Mon { unit = ut, bind = bd }) f as
  = case as of
      []     -> ut []
      -- There are two morphisms of `bd`!!
      (a:as1) -> f a `bd`                  -- `bd` :: m b -> (b -> m [b]) -> m [b]
                 \b -> myMapM m f as1 `bd` -- `bd` :: m [b] -> ([b] -> m [b]) -> m [b]
                 \bs -> ut (b:bs)

-- Data type invariants using rank-2 type
-- Nested data types

data Term v = Var v | App (Term v) (Term v) | Lam (Term (Incr v))
data Incr v = Zero | Succ v

type MapT = forall a b. (a -> b) -> Term a -> Term b

fixMT :: (MapT -> MapT) -> MapT
fixMT f = f (fixMT f)

{-
mapT :: MapT
mapT = fixMT $ \mt f -> \t ->
  case t of
    Var x -> Var (f x)
    App t1 t2 -> App (mt f t1) (mt f t2)
    Lam t -> Lam (mt (mapI f) t)
-}

-- * The key ideas

-- ** Higher-ranked types

-- | Rank
{-
Monotypes τ, σ[0] ::= a    | τ₁ → τ₂
Polytypes σ[n+1]  ::= σ[n] | σ[n] → σ[n+1] | ∀a.σ[n+1]

* determine by resursive induction!
-}

-- | Annotations
f :: (forall a. [a] -> [a]) -> ([Bool], [Char])
f x = (x [True], x ['a', 'b'])
{-
* Local type inference
- propagate type info, avoid redundant type anno
-}

-- | Subsumption:
-- | - "more polymorphic than" relation
{-
k  :: forall a b. a -> b -> b
f1 :: (Int -> Int -> Int) -> Int
f2 :: (forall x. x -> x -> x) -> Int

- k is more poly than f2's arg
--- Some kind of sub-typing!
--- co/contra-variance

Let ≤ be "less poly than":

! (σ₁ → Int) ≥ (σ₂ → Int) ⇐⇒ σ₁ ≤ σ₂      -- WHY


g  :: ((∀b. [b] → [b]) → Int) → Int
k1 :: (∀a. a → a) → Int
k2 :: ([Int] → [Int]) → Int

(g k1) ::: ill-typed!
! (∀a. a → a) → Int ≤ (∀b. [b] → [b]) → Int
! should not apply function with (less poly param) onto (more poly arg)

(g k2) ::: well-typed

-}

