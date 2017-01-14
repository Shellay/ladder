{-# LANGUAGE RankNTypes #-}

{-
Contents from
http://sleepomeno.github.io/blog/2014/02/12/Explaining-Haskell-RankNTypes-for-all/
-}

-- * Review (length)

-- length :: forall a. [a] -> Int

intLength :: [Int] -> Int
intLength = length


-- ** Apply a length-like function to a list

apply :: forall a. ([a] -> Int) -> [a] -> Int
apply f x = f x


-- applyToTuple :: ([a] -> Int) -> ([b], [c]) -> (Int, Int)
-- applyToTuple f (bs, cs) = (f bs, f cs)
-- • Couldn't match ‘b‘ with ‘a‘ ⋯

applyToTuple :: (forall a. [a] -> Int) -> ([b], [c]) -> (Int, Int)
applyToTuple f (bs, cs) = (f bs, f cs)



-- * Explaination

-- applyToTuple :: forall a b c. ([a] -> Int) -> ([b], [c]) -> (Int, Int)
--   where
--     ([a] -> Int) is instantiated only once by (f bs) to ([b] -> Int),
--     thus unable to unify (f cs) for any c
-- vs
-- 
-- applyToTuple :: forall b c. (forall a. [a] -> Int) -> ([b], [c]) -> (Int, Int)
--   where
--     (forall a. [a] -> Int) is instantiated multiple times by (f bs) awa by (f cs)  
