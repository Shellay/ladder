{-
Array in haskell: abstract data types with subscript operation

+ incremental

+ monolithic

-}

import Data.Array


-- * Index types

class (Ord a) => Ix' a where
  range'   :: (a, a) -> [a]
  index'   :: (a, a) -> a -> Int
  inRange' :: (a, a) -> a -> Bool
  -- where (a, a) are *inclusive* bound


oneToFour = range (0,4)

zzToOneTwo = range ((0,0),(1,2))


-- * Array creation

-- ** Using bounds (a,a) and enumerated list [(a,b)]
-- array :: (Ix a) => (a,a) -> [(a,b)] -> Array a b

{-
λ> array (0,2) [(0,"a"),(1,"b"),(2,"c")]
array (0,2) [(0,"a"),(1,"b"),(2,"c")]

-- order of indices won't matter
λ> array (0,2) [(0,"a"),(2,"c"),(1,"b")]
array (0,2) [(0,"a"),(1,"b"),(2,"c")]

λ> array (0,2) [(0,"a"),(1,"b"),(2,"c"),(3,"d")]
array *** Exception: Ix{Integer}.index: Index (3) out of range ((0,2))

λ> array (0,4) [(0,"a"),(1,"b"),(2,"c"),(3,"d")]
array (0,4) [(0,"a"),(1,"b"),(2,"c"),(3,"d"),(4, ...)]
  *** Exception: (Array.!): undefined array element
-}

squares = array (1,100) [(i,i*i) | i <- [1..100]]


-- ** Subscripting using (!)

num49 = squares!7

mkArray :: (Ix i) => (i -> b) -> (i,i) -> Array i b
mkArray f bnds = array bnds [(i, f i) | i <- range bnds]


-- recursively

fibs :: Int -> Array Int Int
fibs n = a
  where a = array (0,n) ([(0,1), (1,1)] ++
                         [(i, a!(i-2) + a!(i-1)) | i <- [2..n]])


-- * Accumulation

-- accumArray :: (Ix i) => (e -> a -> e) -> e -> (i, i) -> [(i, a)] -> Array i e
-- accumArray reduFunc initVal bounds alist

hist :: (Ix i, Integral x) => (i,i) -> [i] -> Array i x
hist bnds labels = accumArray (+) 0 bnds [(l,1) | l <- labels, inRange bnds l]

decades :: (RealFrac a) => a -> a -> [a] -> Array Int Int
decades start end = hist (0,9) . map decade
  where decade year = floor $ (year - start) * loc
        loc         = 10 / (end - start)

dis = hist (0,3) [2,2,1,3,3,2,2]

groupBy :: (Ix i) => (a -> i) -> (i,i) -> [a] -> Array i [a]
groupBy key bnds = accumArray (flip (:)) [] bnds . map (\a -> (key a, a))

stuff = groupBy (`mod` 3) (0,3) [1,2,3,0,9,8,7]


-- * Incremental updates
-- (//) :: (Ix a) => Array a b -> [(a,b)] -> Array a b

-- swap two rows of a matrix
swapRows :: (Ix a, Ix b, Enum b) => a -> a -> Array (a,b) c -> Array (a,b) c
swapRows i j a = a // ([( (i,k), a!(j,k) ) | k <- [minCol..maxCol]] ++
                       [( (j,k), a!(i,k) ) | k <- [minCol..maxCol]])
                 where ((_, minCol), (_, maxCol)) = bounds a
-- very inefficient due to generating immutable intermediate values...

-- BUT there are *loop fusion optimizations*!


-- accum :: Ix i => (e -> a -> e) -> Array i e -> [(i, a)] -> Array i e



-- * E.g. Matrix multiplication

matMult :: (Ix i, Ix j, Ix k, Num d) =>
  Array (i, k) d -> Array (k, j) d -> Array (i, j) d
matMult x y = array rsltBnds
              [( (i, j), sum [x!(i,k) * y!(k,j) | k <- range (kLow, kUp)])
              | i <- range (iLow, iUp), j <- range (jLow, jUp)]
  where ( (iLow, kLow), (iUp, kUp) ) = bounds x
        ( (kLow', jLow), (kUp', jUp) ) = bounds y
        rsltBnds | (kLow', kUp') == (kLow, kUp) = ((iLow, jLow), (iUp, jUp))
                 | otherwise                    = error "matMult: wrong size"

-- can use accumArray
matMult' :: (Ix i, Ix j, Ix k, Num d) =>
  Array (i, k) d -> Array (k, j) d -> Array (i, j) d
matMult' x y = accumArray (+) 0 rsltBnds
               [( (i, j), x!(i,k) * y!(k,j) )
               | i <- range (iLow, iUp),
                 j <- range (jLow, jUp),
                 k <- range (kLow, kUp)
               ]
  where ((iLow, kLow), (iUp, kUp)) = bounds x
        ((kLow',jLow), (kUp',jUp)) = bounds y
        rsltBnds | (kLow, kUp) == (kLow', kUp') = ((iLow, jLow), (iUp, jUp))
                 | otherwise                    = error "matMult: wrong size"

-- generalize with (+) and (*) ops
genMatMult :: (Ix i, Ix j, Ix k) =>
  e -> (e -> a -> e) -> (b -> c -> a) ->
  Array (i, k) b -> Array (k, j) c -> Array (i, j) e
genMatMult zero merge star x y =
  accumArray merge zero rsltBnds
               [( (i, j), (x!(i,k)) `star` (y!(k,j)) )
               | i <- range (iLow, iUp),
                 j <- range (jLow, jUp),
                 k <- range (kLow, kUp)
               ]
  where ((iLow, kLow), (iUp, kUp)) = bounds x
        ((kLow',jLow), (kUp',jUp)) = bounds y
        rsltBnds | (kLow, kUp) == (kLow', kUp') = ((iLow, jLow), (iUp, jUp))
                 | otherwise                    = error "matMult: wrong size"
