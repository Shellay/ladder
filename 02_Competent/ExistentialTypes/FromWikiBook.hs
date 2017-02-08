{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ExistentialQuantification #-}

-- *** The `forall` keyword *** --
-- * Universal quatification
{- like
map :: forall a b. (a -> b) -> [a] -> [b]
id :: forall a. a -> a
-}
-- * Addtional constraints P(x)
-- for ad-hoc interface restriction
-- e.g. ∃x,P(x), ∀x,P(x)



-- *** E.G. heterogeneous lists *** –

data BoxShowable = forall s. Show s => SB s

heteroListOfShowable :: [BoxShowable]
heteroListOfShowable = [SB (), SB 5, SB True]

instance Show BoxShowable where
  show (SB s) = "BoxWith{" ++ show s ++ "}"


-- main = print heteroListOfShowable
main = mapM_ print heteroListOfShowable
-- main = mapM_ (putStr . show) heteroListOfShowable


-- *** A further explanation *** --

-- * `forall`, types as SET of possible values
--   e.g. Bool == {True, False, ⊥},
--   where ⊥ is member of every type in Haskell (forall a, HasBottom a => ...)

-- * `forall` asserts commonality/intersection
--   e.g. (x :: forall a. a)
--        i.e. forall a. (x :: a)
--        means value x conforms to the INTERSECTION of all types
--        where a is each type
--   e.g. (x :: forall a. Show a => a)
--        i.e. forall a. Show a => (x :: a)
--        means value x conforms to the INTERSECTION of all (Show) types,
--        where a is each (Show a) type

-- e.g.
-- xs :: [forall a. a]           --> ∀x∈xs.(∀a. x :: a),            i.e. x == ⊥
-- xs :: [forall a. Show a => a] --> ∀x∈xs.(∀a.(Show a) => x :: a), i.e. x is showable
-- xs :: [forall a. Num a => a]  --> ∀x∈xs.(∀a.(Num a) => x :: a),  i.e. x is number
-- xs :: forall a. [a]           --> ∀a.(xs :: [a]),                i.e. xs == [⊥, ...]


-- REALTION to subsumption??? universal vs existential ??


data T = forall a. MkT a
-- where MkT :: forall a. (a -> T)

-- * but pattern matching?

-- foo (MkT x) = ... --> x :: ?

tList = [MkT 5, MkT (), MkT True, MkT map] :: [T]

-- We can do any useful thing with such an unconstrained x
--     except reduce them to WHNF


data T' = forall a. Show a => MkT' a 
instance (Show T') where
  show (MkT' a) = "T'{" ++ show a ++ "}"

tList' = [MkT' 5, MkT' (), MkT' True, MkT' "Sartre"] :: [T']



-- *** E.g. runST *** --
-- runST :: forall a. (forall s. ST s a) -> a



-- *** Quantification as primitive *** --

newtype Pair a b = Pair {runPair :: forall c. (a -> b -> c) -> c}

makePair :: a -> b -> Pair a b
makePair a b = Pair $ \f -> f a b
  
pairFirst :: Pair a b -> a
pairFirst (Pair rp) = rp $ \x y -> x

pairSecond :: Pair a b -> b
pairSecond p = runPair p $ \x y -> y

getTuple :: Pair a b -> (a, b)
getTuple (Pair rp) = rp $ \x y -> (x,y)

p = makePair "a" 123
