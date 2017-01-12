{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-

Functional dependencies are used to *constrain* parameters of type
classes where *one* of parameters can be determined from the *others*.

-}


-- * Examples

-- ** Direct style

data Vector = Vector Int Int deriving (Eq, Show)
data Matrix = Matrix Vector Vector deriving (Eq, Show)

{- 
instance Num Vector where
  Vector a1 b1 + Vector a2 b2 = Vector (a1+a2) (b1+b2)
  Vector a1 b1 - Vector a2 b2 = Vector (a1-a2) (b1-b2)
  {- ... and so on ... -}
 
instance Num Matrix where
  Matrix a1 b1 + Matrix a2 b2 = Matrix (a1+a2) (b1+b2)
  Matrix a1 b1 - Matrix a2 b2 = Matrix (a1-a2) (b1-b2)
  {- ... and so on ... -}
-}

-- *** Serious problem when overloading (*) op
{-
  (*) :: Matrix -> Matrix -> Matrix
  (*) :: Matrix -> Vector -> Vector
  (*) :: Matrix -> Int -> Matrix
  (*) :: Int -> Matrix -> Matrix
  {- ... and so on ... -}
-}

-- ** If with typeclass (Mult) using {LANGUAGE MultiParamTypeClasses}

class Mult0 a b c where
  (.*) :: a -> b -> c

translate :: Matrix -> Matrix
translate (Matrix (Vector a b) (Vector c d)) = Matrix (Vector a c) (Vector b d)

instance Mult0 Matrix Matrix Matrix where
  m1 .* m2 = let (Matrix w z) = translate m2
                 nT = Matrix (m1 .* w) (m1 .* z)
             in translate nT

instance Mult0 Matrix Vector Vector where
  (Matrix u v) .* w = Vector (u .* w) (v .* w)

instance Mult0 Vector Vector Int where
  (Vector a b) .* (Vector c d) = a * c + b * d


-- ** E.g.

u = Vector 1 2
v = Vector 3 4
m1 = Matrix u v
m2 = Matrix (Vector 5 6) (Vector 7 8)
m3 = Matrix (Vector 4 3) (Vector 2 1)

-- uv = u .* v -- type error: must explicitly specify which type results
-- since there maybe multiple possible c due to
-- (instance Mult Vector Vector c where ...)
uv = u .* v :: Int

-- m1, m2, m3 :: Matrix
-- (m1 * m2) * m3              -- type error: type of (m1*m2) is ambiguous
-- (m1 * m2) :: Matrix * m3    -- this is ok
m12 = (m1 .* m2) :: Matrix
m123 = (m12 .* m3) :: Matrix


-- *** Also bad to allow unintended instances

-- instance Mult Matrix Matrix (Maybe Char) where blabla


-- ** Fundep to allow only ONE possible value of (c) given (a) and (b)

class Mult a b c | a b -> c where
  (..*) :: a -> b -> c

instance Mult Vector Vector Int where
  (Vector x1 y1) ..* (Vector x2 y2) = x1*x2 + y1*y2

instance Mult Matrix Vector Vector where
  (Matrix u1 u2) ..* v = Vector (u1 ..* v) (u2 ..* v)

instance Mult Matrix Matrix Matrix where
  m1 ..* m2 = let (Matrix w z) = translate m2
                  nT = Matrix (m1 .* w) (m1 .* z)
              in translate nT
  
n1 = m1
n2 = m2
n12 = n1 ..* n2
n3 = m3
n123 = (n12 ..* n3)
-- All OK



-- * Another example

class Extract' container elem where
  extract' :: container -> elem

instance Extract' (a,b) a where
  extract' (x,_) = x

-- Source of ambiguity for (extract')
instance Extract' (a,b) Bool where
  extract' (x,_) = True

-- Cannot specify which version of (extract') should be used.
-- vx = extract' ('x', 3)

class Extract container elem | container -> elem where
  extract :: container -> elem

instance Extract (a,b) a where
  extract (x,_) = x

vx = extract ('x', 3)
