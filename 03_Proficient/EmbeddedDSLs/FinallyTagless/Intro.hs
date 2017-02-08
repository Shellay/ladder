{- From Object Algebras to Finally Tagless Interpreters
https://oleksandrmanzyuk.wordpress.com/2014/06/18/from-object-algebras-to-finally-tagless-interpreters-2/
-}

-- * Overview

-- object algebra
-- finally tagless interpreters


-- * Expression problem

data Exp = Lit Int
         | Add Exp Exp

e1 = Add (Lit 1)
         (Add (Lit 2)
              (Lit 3))

eval :: Exp -> Int
eval (Lit n)   = n
eval (Add x y) = eval x + eval y

view :: Exp -> String
view (Lit n)   = show n
view (Add x y) = "(" ++ view x ++ " + " ++ view y ++ ")"


-- To extend the language with (*)-op?

-- + new constructor!
-- 
--   data Exp |= Mul Exp Exp

-- + new case to every related function!
--
-- eval (Mul Exp Exp) = ...


-- e.g. in OOP like java/scala
{-
trait Exp {
  def eval: Int
}

class Lit(n: Int) extends Exp {
  override def eval = n
}

class Add(x: Exp, y: Exp) extends Exp {
  override def eval = x.eval + y.eval
}
-}

-- Adding a subtype is easy!
{-
class Mul(x: Exp, y: Exp) extends Exp {
  override def eval = x.eval * y.eval
}
-}

-- Adding a method is hard!


-- ** Analogue to matrix(subtypes * methods)

-- FP - adding a row/subtype is hard
-- + changing type definition
-- + add cases to each function

-- FP - adding a col/method is easy
-- + simply writing one new function block

{-
      eval  view
Lit    |     |
Add    |     |
Mul    +-----+
-}


-- OOP - adding a column/method is hard
-- + add methods to each subtype
-- + changing basic interface

-- OOP - adding a row/subtype is easy
-- + simply writing one new class block

{-
      eval  view
Lit    ------+
Add    ------+
Mul    ------+
-}

-- *** Duality



-- * Object algebras

-- abstract factory

class ExpAlg t where
  lit :: Int -> t
  add :: t -> t -> t

-- concrete factory

e2 :: ExpAlg t => t             -- No concrete type: i.e. tagless.
e2 = lit 1 `add` (lit 2 `add` lit 3)
