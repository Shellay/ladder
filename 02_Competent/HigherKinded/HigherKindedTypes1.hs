{-# LANGUAGE GADTs, KindSignatures, DataKinds, TypeInType #-}
-- NB: With TypeInType, you must import * from Data.Kind

import Data.Kind

{-
data Ze
data Su n

data Vec :: * -> * -> * where
  Nil  :: Vec a Ze
  Cons :: a -> Vec a n -> Vec a (Su n)

-- We do not intend to use sth like (Vec Int Char)
-}


-- Using DataKinds for datatype promotion

data Nat = Ze | Su Nat
data Vec :: * -> Nat -> * where -- `Nat` is promoted as a Kind expression
  EmptyVec :: Vec a 'Ze              -- `'Ze` 
  ProperVec :: a -> Vec a n -> Vec a ('Su n)

{-
Nat :: *
'Zero :: Nat
'Succ :: Nat -> Nat
-}


{- * Kind polymorphism
   - TypeInType
-}

data Proxy a = Proxy
data App f a = MkApp (f a)      -- App :: forall k. (k -> *) -> (k -> *)

x :: Proxy ('MkApp ('Just 'True)) -- Proxy (App Maybe Bool)
x = Proxy

data P = MkP
data Prom = P

data List a = Nil | Cons a (List a)
data Pair a b = Pair a b
data Sum a b = L a | R b

{- * With -XDataKinds, [] and () are natively promoted to kinds!
-}



main :: IO ()
main = return ()

