{-# LANGUAGE GADTs #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}


-- * An associated type synonym example

-- alternative to fundeps


-- ** The ‘class’ declaration

-- fundep style
{-
class Collects e ce | ce -> e where
  empty  :: ce
  insert :: e -> ce -> ce
  member :: e -> ce -> Bool
  toList :: ce -> [e]
-}

-- type synonym style

class Collects ce where
  type Elem ce                  -- Elem :: * -> *
  -- avoid verbosity to make data instance for (data Elem ce)
  empty  :: ce
  insert :: Elem ce -> ce -> ce
  member :: Elem ce -> ce -> Bool
  toList :: ce -> [Elem ce]


-- ** An instance

-- crsp. fundep style
{-
instance Eq e => Collects e [e] where
  empty = []
  insert = (:)
  member = elem
  toList = id
-}

instance Eq e => Collects [e] where
  type Elem [e] = e
  empty = []
  insert = (:)
  member = elem
  toList = id


-- ** Using generic collections

-- crsp. fundep style
{-
sumCollects :: (Collects e c1, Collects e c2) =>
  c1 -> c2 -> c2
sumCollects c1 c2 = foldr insert c2 (toList c1)
-}

sumCollects ::
  (Collects c1, Collects c2, Elem c1 ~ Elem c2) =>
  c1 -> c2 -> c2
sumCollects c1 c2 = foldr insert c2 (toList c1)



-- * Detailed definition of type synonym families

-- 2 flavours
-- + toplevel
-- + inside type classes

-- ** Family declarations

type family Elem' c :: *

type family F a b :: * -> *

{-
F Char [Int]       -- OK!  Kind: * -> *
F Char [Int] Bool  -- OK!  Kind: *
F IO Bool          -- WRONG: kind mismatch in the first argument
F Bool             -- WRONG: unsaturated application
-}

-- open vs closed

type family G a where
  G Int = Bool
  G a   = Char

-- *** Associated family declarations

-- named type params must be perm of class params

{-
class C a b c where { type T c a :: * }
-- OK
class D a where { type T a x :: * }
-- No: x is not a class parameter
class D a where { type T a :: * -> * }
-- OK
-}

-- ** Type instance declarations

{-
type family F a :: *
type instance F [Int]              = Int         -- OK!
type instance F String             = Char        -- OK!
type instance F (F a)              = a           -- WRONG: type parameter mentions a type family
type instance F (forall a. (a, b)) = b           -- WRONG: a forall type appears in a type parameter
type instance F Float              = forall a.a  -- WRONG: right-hand side may not be a forall type
 
type family F2 a where                           -- OK!
  F2 (Maybe Int)  = Int
  F2 (Maybe Bool) = Bool
  F2 (Maybe a)    = String
 
type family G a b :: * -> *
type instance G Int            = (,)     -- WRONG: must be two type parameters
type instance G Int Char Float = Double  -- WRONG: must be two type parameters
-}

-- *** Closed family simplification

-- choose to match target
-- + subst LHS
-- + apart or compatible

-- for equations a, b
--
-- a `apart` b
--   where simpl(a == b) == false
--
-- a `compatible` b
--   where case unify(a.LHS, b.LHS, env) of
--     Success subst -> unify(a.RHS, b.RHS, subst ++ env)
--     Fail -> Fail

{-
type family F a where
  F Int  = Bool                 -- 1
  F Bool = Char                 -- 2
  F a    = Bool                 -- 3
-- where (1 `compa` 2) and (1 `compa` 3) but
--       not (2 `compa` 3)
 
type family And (a :: Bool) (b :: Bool) :: Bool where
  And False c     = False
  And True  d     = d
  And e     False = False
  And f     True  = f
  And g     g     = g
-- where all pairs are compatible
-}


-- e.g. unsafeCoerce
{-
type family J a b where
  J a a = Int
  J a b = Bool
-}


-- GHC no internal notion of inequality
-- with GADT e.g.
data H  :: * -> * where
  HInt  :: H Int
  HBool :: H Bool

type family Foo (a :: *) :: * where
  Foo Int = Char
  Foo a   = Double

bar :: H a -> Foo a
bar HInt = 'x'                  -- with (a == Int)
-- bar _    = (3.14 :: Double)     -- can't assure (a != Int)
bar HBool = (3.14 :: Double) -- OK.


-- *** Associated type instances

-- consistency of instance parameters with family parameters

-- *** Overlap

-- for type-safety rather than consistency
{-
type instance F (a, Int) = [a]
type instance F (Int, b) = [b]   -- overlap permitted
 
type instance G (a, Int)  = [a]
type instance G (Char, a) = [a]  -- ILLEGAL overlap, as [Char] /= [Int]
-- consistent, but tending to be type-unsafe
-}

-- *** Decidability

{- 

type instance F t1 .. tn = t
  where for (G s1 .. sm)
    + for ctor in si: ctor is not type family ctor
    + |symbols(s1 .. sm)| < |symbols(t1 .. tm)|
      where symbols == ctor | type-var
    + for type-var a: number(s1..sm, a) <= number(t1..tm, a)

-}

-- no completeness of type inference

-- loopy

-- -XUndecidableInstances to allow recursive type and possible
-- non-termination


-- ** Equality constraints

-- with form (t1 ~ t2)
-- t1, t2 are arbitrary monotypes w.n.r.t. higher rank types

{-
sumCollects :: 
  (Collects c1, Collects c2, Elem c1 ~ Elem c2) => 
  c1 -> c2 -> c2
-}

class C a b | a -> b

-- can be rewritten to

class (F' a ~ b) => C' a b where
  type F' a

-- fundep (a1 .. an -> b) represented by
-- type family (F a1 .. an) and superclass context equality
-- (F a1 .. an ~ b)
-- i.e. giving name to fundep



-- * Frequently asked questions

-- ** type families vs fundeps

-- ** Injectivity, type inference, and ambiguity

{-

type family P a
p :: P a -> P a
p = undefined

-- Error: ‘P’ is a type func, and may not be injective
-}

-- The problem: (F t1 == F t2) does not imply (t1 == t2)
-- i.e. non-injective

-- The solution:
-- + every type-var after ‘=>’
-- + every type-var appear outside type-func call

-- Or use data families rather than type families

data family P a
p :: P a -> P a
p = undefined

q :: P Int -> P Int
q x = p x
