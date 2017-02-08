module Comonad where


class Comonad w where
  extract :: w a -> a
  extend :: (w a -> b) -> (w a -> w b)

  (=>=) :: (w a -> b) -> (w b -> c) -> (w a -> c)
  f =>= g = \w -> g (extend f w)

  -- * Laws (CoKleisli)
  -- 
  -- | COUNIT (cat-identity)
  -- extract =>= f  ==  f
  -- f =>= extract  ==  f
  --
  -- | ASSOC (cat-comp-assoc)
  -- (f =>= g) =>= h  ==  f =>= (g =>= h)



-- * OOP

