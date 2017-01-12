{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

import qualified Data.IntMap
import qualified Data.Char (ord, chr)
import Control.Applicative ((<|>))

import Prelude hiding (lookup)

{-

+ Indexed type families

+ ad-hoc *overloading* of data types

+ analogue of type classes

+ useful for
  - generic programming
  - highly parameterised lib interfaces
  - interfaces with enhanced static info (like dep-types)

+ 2 families
  - data families
  - type synonym families
-}


-- * What are type families

-- partial function at type level

-- compute what data constructors to operate on (rather than fixed)

-- treated as opaque unknowns (cf. parameterically polymorphic types)

-- interface declaration: *kind* and *arity*
-- instance definition: application on *part-of-domain*

-- ? dispatching constructors w.r.t. type parameters


data family XList a

data instance XList Char
  = XCons !Char !(XList Char)
  | XNil

data instance XList ()
  = XListUnit !Int


-- ** versus GADTs
-- ? GADT ctors must be put together
-- ! Type families can be extended

-- ** versus FunDeps
-- = equivalently expressive
-- ? FunDep is *relational* style
-- ! Type familiy is *functional* style at *type level*



-- * What is needed to use?

-- with pragma {-# LANGUAGE TypeFamilies #-}



-- * An associated data type example
-- with *generalised tries*

-- ** Declaration

class GMapKey k where
  data GMap k :: * -> *
    -- where
    --   + (k) associated in (GMap k) is constrained by (GMapKey k).
    --   + (GMap k) is parametrisable with (* -> *).
  empty  :: GMap k v
  lookup :: k -> GMap k v -> Maybe v
  insert :: k -> v -> GMap k v -> GMap k v


-- ** An (Int) instance

instance GMapKey Int where
  data GMap Int v        = GMapInt (Data.IntMap.IntMap v)
    -- where
    --   ! GADT syntax is yet supported.
  empty                  = GMapInt Data.IntMap.empty
  lookup k (GMapInt m)   = Data.IntMap.lookup k m
  insert k v (GMapInt m) = GMapInt (Data.IntMap.insert k v m)


instance GMapKey Char where
  data GMap Char v        = GMapChar (GMap Int v)
  empty                   = GMapChar empty
  -- lookup k (GMapChar gmi) = lookup (Data.Char.ord k) gmi
  -- !!! This does not work without (hiding Prelude.lookup).
  -- !!! - cannot resolve ambiguity with (Prelude.lookup)
  lookup k (GMapChar (GMapInt mi)) =
    Data.IntMap.lookup (Data.Char.ord k) mi
  insert k v (GMapChar mInt) =
    GMapChar $ insert (Data.Char.ord k) v mInt


-- ** A unit instance

instance GMapKey () where
  data GMap () v = GMapUnit (Maybe v)
  empty = GMapUnit Nothing
  lookup () (GMapUnit v) = v
  insert () v (GMapUnit _) = GMapUnit $ Just v


-- ** Product and sum instances

-- product type with Tuple (Pair)
instance (GMapKey a, GMapKey b) => GMapKey (a, b) where
  data GMap (a, b) v = GMapPair (GMap a (GMap b v))
  empty = GMapPair $ empty      -- empty :: GMap a w
  lookup (a, b) (GMapPair gmav) = lookup a gmav >>= lookup b
  insert (a, b) v (GMapPair gmav) =
    -- GMapPair $ case lookup a gmav of
    --              Just gmbv -> insert a (insert b v gmbv) gmav
    --              Nothing   -> insert a (insert b v empty) gmav
    GMapPair $ insert a (insert b v gmbv) gmav
    where (Just gmbv) = lookup a gmav <|> Just empty

-- sum type with Either
instance (GMapKey a, GMapKey b) => GMapKey (Either a b) where
  data GMap (Either a b) v = GMapEither (GMap a v) (GMap b v)
  empty = GMapEither empty empty
  lookup e (GMapEither gma gmb) =
    case e of
      Left  a -> lookup a gma
      Right b -> lookup b gmb
  insert e v (GMapEither gma gmb) =
    case e of
      Left  a -> GMapEither (insert a v gma) gmb
      Right b -> GMapEither gma (insert b v gmb)


-- ** Using a generic map

myGMap :: GMap (Int, Either Char ()) String
myGMap = insert (5, Left 'c') "(5, Left 'c')"    $
         insert (4, Right ()) "(4, Right ())"    $
         insert (5, Right ()) "This is the one!" $
         insert (5, Right ()) "This is the two!" $
         insert (6, Right ()) "(6, Right ())"    $
         insert (5, Left 'a') "(5, Left 'a')"    $
         empty



-- * Detailed definition of data families

-- 2 flavours

-- + toplevel
-- + inside type classes (associated types)


-- ** Family declarations

data family Array1 e

data family Array2 :: * -> *


-- *** Associated family declarations

class MyMapKey k where
  data MyMap k :: * -> *
    -- arg names must be permu of class vars

class C a b c where { data CC c a :: * }  
-- OK
-- class C' a b c where { data CC' a a :: * } 
-- Bad: repeated variable
class D a where { data DD a x :: * }      
-- Bad: x is not a class variable - but permitted by compiler!
class D' a where { data DD' a :: * -> * }  
-- OK


-- ** Instance declarations

-- keyword ‘data instance’

data family T a
data instance T Int  = A
data instance T Char = B
-- nonsense :: T a -> Int
-- nonsense A = 1             -- WRONG: These two equations together...
-- nonsense B = 2             -- ...will produce a type error.


-- *** Associated type instances


-- *** Scoping of class parameters

class Foo a b where
  data Bar a

-- instance Foo [c] d where
--   data Bar [c] = MkBar (c, d)
--     ! The RHS of an associated type declaration mentions ‘d’
--     ! All such variables must be bound on the LHS

-- Needs FlexibleInstances for [c]
instance Foo [c] d where
  data Bar [c] = MkBar c


-- *** Type class instances of family instances

-- *** Overlap

-- Instance declarations of a data family may not overlap at all.



-- ** Import and export

-- Visibility of *data instance* determines visibility of *data
-- ctor*

-- Versus type class: ‘T(..)’ imports all ctors introduced

-- import GMap(GMapEither)


-- *** Associated families

-- import GMapKey(type GMap, empty, lookup, insert)

-- *** Examples

{-
module GMap (GMapKey) where...
: Exports just the class name.

module GMap (GMapKey(..)) where...
: Exports the class, the associated type GMap and the member functions
empty, lookup, and insert . None of the data constructors is exported.

module GMap (GMapKey(..), GMap(..)) where...
: As before, but also exports all the data constructors GMapInt ,
GMapChar , GMapUnit , GMapPair , and GMapEither .

module GMap (GMapKey(empty, lookup, insert), GMap(..)) where...
: As before.

module GMap (GMapKey, empty, lookup, insert, GMap(..)) where...
: As before. 
-}

-- *** Instances

-- Family instances are implictly exported, like class instances.

-- Only *heads* of instances, not data ctors they define.


