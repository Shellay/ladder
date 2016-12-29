-- * Infinite data structure

fib :: [Int]
fib = 0 : 1 : zipWith (+) fib (tail fib)

sumSoFar x [] = [x]
sumSoFar x (y:ys) = x : sumSoFar (x+y) ys

s = sumSoFar 0 [1,3,5,7]

{- * Virtual stuff

data [a] = [] | a : [a]

-}
