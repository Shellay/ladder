{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE Arrows #-}

-- * Arrow tutorial

-- | Arrow : Alternative of structuring computations like functor
-- classes.


-- * Stephen's Arrow Tutorial

module Tutorial where

import Control.Arrow
import Control.Monad
import qualified Control.Category as Cat
import Data.List
import Data.Maybe
import System.Random

-- main :: IO ()
-- main = return ()

-- can visualize arrows as circuits



-- * Type definition for 'Circuit'

newtype Circuit a b = Circuit { unCircuit :: a -> (Circuit a b, b) }

{- | Definition of 'Category'
@
class Category (cat :: k -> k -> *) where
  id  :: forall (a :: k). cat a a
  (.) :: forall (b :: k) (c :: k) (a :: k).
           cat b c -> cat a b -> cat a c
@
-}

instance Cat.Category Circuit where
  -- Circuit :: k -> k -> *

  -- id  :: cat a a
  id = Circuit $ \a -> (Cat.id, a)

  -- (.) :: cat b c -> cat a b -> cat a c
  -- (.) = dot
  --   where
  --     Circuit bc `dot` Circuit ab =
  --       Circuit $ \a -> let (cAB, b) = ab a
  --                           (cBC, c) = bc b
  --                       in  (cBC `dot` cAB, c)
  (.) = dot
    where
      cBC `dot` cAB = Circuit $ \a -> let (cAB1, b) = unCircuit cAB a
                                          (cBC1, c) = unCircuit cBC b
                                      in  (cBC1 `dot` cAB1, c)


instance Arrow Circuit where
  arr f = Circuit $ \a -> (arr f, f a)

  -- first (Circuit cir) = Circuit $ \(b, d) ->
  --   let (cir', c) = cir b
  --   in  (first cir', (c, d))

  -- first :: Arrow r => r b c -> r (b, d) (c, d)
  first cBC = Circuit $ \(b, d) -> let (cBC₁, c) = unCircuit cBC b
                                   in  (first cBC₁, (c, d))


runCircuit0 :: Circuit a b -> [a] -> [b]
runCircuit0 _ [] = []
runCircuit0 cAB (a:as) =           -- map st (z:zs) =
  let (cAB', a') = unCircuit cAB a --   let (st', z') = (runState st) z
  in  a' : runCircuit cAB' as      --   in  (z' : map st' zs)

runCircuit1 :: Traversable t => Circuit a b -> t a -> t b
runCircuit1 cAB as =
  snd $ mapAccumL (\cAB a -> unCircuit cAB a) cAB as
  -- where
  --   mapAccumL :: Traversable t =>
  --     (a₁ -> b -> (a₂, c)) -> a₀ -> t b -> (aₙ, t c)
  --       spec to
  --     (a₁ -> a₂ -> (a₃, b)) -> a₀ -> t aᵢ -> (aₙ, t b)

runCircuit :: Traversable t => Circuit a b -> t a -> t b
runCircuit cir = snd . mapAccumL unCircuit cir



-- * 'Circuit' primitives

-- generalized accumulator

-- | Accumulator that outputs a value determined by the supplied
-- function.
accum :: acc -> (a -> acc -> (b, acc)) -> Circuit a b
accum acc f = Circuit $ \inp ->
  let (oup, acc₁) = inp `f` acc
  in  (accum acc₁ f, oup)
  
accumF :: (a -> acc -> (b, acc)) -> acc -> Circuit a b
accumF f = g
  where
    g s = Circuit $ \a ->       -- state s = \s -> 
      let (b, s₁) = a `f` s     --   let (b, s₁) = (runState f a) s
      in  (g s₁, b)             --   in  (state s₁, b)

-- | Accumulator that outputs the accumulator value.
accum' :: s -> (a -> s -> s) -> Circuit a s
accum' s₀ f = accum s₀ (\a s₁ -> let s₂ = (f a) s₁ in (s₂, s₂))

accumF' :: (a -> s -> s) -> s -> Circuit a s
accumF' f = accumF (\a s₁ -> let s₂ = f a s₁ in (s₂, s₂))
  -- where
  --   accumF :: (a -> acc -> (b, acc)) -> acc -> Circuit a b
  --     spec to
  --   accumF :: (a -> s₁ -> (s₂, s₂)) -> s₀ -> Circuit a s

total :: Num a => Circuit a a
total = accum' 0 (+)

x = runCircuit total [1,0,1,0,0,2]
-- | Note the method 'runCircuit' above traverses through a list.
-- runCircuit :: Circuit a b -> [a] -> [b]
-- cir        :: Circuit a b === a -> (Circuit a b, b)
-- runCircuit cir :: [a] -> [b]

data Tree a = Tree a [Tree a]
  deriving (Functor, Foldable, Traversable, Show)
-- | Automatic Derivation of 'Foldable' targets pre-order.

t1 = Tree 1 [Tree 2 [],
             Tree 3 [Tree 4 [],
                     Tree 5 [],
                     Tree 6 []]]
sumT1 = runCircuit total t1

-- * Arrow 'proc' notation

mean1 :: Fractional a => Circuit a a
mean1 = (total &&& (arr (const 1) >>> total)) >>> arr (uncurry (/))
        -- where
        --   arr (const 1) :: (Arrow a, Num n) => a b n
        --     spec to
        --   arr (const 1) :: (Num n) => Circuit n n
        --   
        --   total         :: (Num n) => Circuit n n
        -- 
        --   arr (uncurry (/)) :: (Arrow a, Fractional c) => a (c, c) c
        --     spec to
        --   arr (uncurry (/)) :: (Num n) => Circuit (n, n) n
        -- 
        --   total &&& (arr (const 1) >>> total)
        --     :: (Num c', Num c) => Circuit c (c, c')
        --   
        --   (&&&) :: Circuit b c -> Circuit b c' -> Circuit b (c, c')
        --         -- makes a tupled-arrow with two parallel arrows
        --   (>>>) :: cat a b -> cat b c -> cat a c
        --         -- reversed composition, i.e. pipes to, feeds to, let flowing to

{- | proc block:

- <var-bind-pat> <- <arrow> -< <pure-expr>

-}

mean2 :: Fractional a => Circuit a a
mean2 = proc a -> do          -- entrance of arrow, here (\a -> ...) inside 'Circuit'
  t <- total -< a            -- parallel making fst component (binding to entrance)
  n <- total -< 1            -- parallel making snd component
  arr (uncurry (/)) -< (t, n) -- merges components, keeps on feeding


mean3 :: Fractional a => Circuit a a
mean3 = proc a -> do
  t <- total -< a
  n <- total -< 1
  returnA -< t / n

meanT1 = runCircuit mean3 t1

-- analogue with native arrow (->)

(.>) :: (a -> b) -> (b -> c) -> (a -> c)
(.>) = flip (.)
infix 5 .>
  
(&) :: (a -> b) -> (a -> c) -> a -> (b, c)
(&) f g = \a -> (f a, g a)
infix 6 &

total0 :: Num a => [a] -> a
total0 = foldr (+) 0

mean4 :: Fractional a => [a] -> a
mean4 =
  let
    path1 = total0
    path2 = map (const 1) .> total0
    comp  = path1 & path2       -- :: [a] -> (a, a)
  in
    comp .> (uncurry (/))

mean5 :: Fractional a => [a] -> a
mean5 = total0 & (fmap (const 1) .> total0) .> (uncurry (/))


-- * Hangman: Pick a word

generator :: Random a => (a, a) -> StdGen -> Circuit () a
generator bnd gen = accum gen $ \() gen -> randomR bnd gen
  -- where
  --   accum :: s -> (a -> s -> (b, s)) -> Circuit a b
  --     spec to
  --   accum :: StdGen -> (() -> StdGen -> (a, StdGen)) -> Circuit () a

dictionary = ["dog", "cat", "bird"]

pickWord :: StdGen -> Circuit () String
pickWord gen = proc () -> do
  idx <- generator (0, length dictionary - 1) gen -< ()
  returnA -< dictionary !! idx

example :: Int -> IO ()
example n = do
  gen <- getStdGen
  let
    rs = runCircuit (pickWord gen) (take n $ repeat ())
  print rs


-- little arrows

oneShot :: Circuit () Bool
oneShot = accum True $ \() s -> (s, False)
  -- where
  --   accum :: acc -> (a -> acc -> (b, acc)) -> Circuit a b
  --     spec to
  --   accum :: Bool -> (() -> Bool -> (Bool, Bool)) -> Circuit () Bool
  
-- A "shape" list is needed for using 'runCircuit oneShot'!
-- runCircuit oneShot :: t a -> t Bool

delayedEcho :: a -> Circuit a a
delayedEcho a₀ = accum a₀ $ \aNew aCurr -> (aCurr, aNew)
  -- where
  -- 
  --   accum :: acc -> (a -> acc -> (a, acc)) -> Circuit a a
  --   
  --   with step function '\aNew aCurr -> (aCurr, aNew)', the fetched
  --   input value 'aNew' is "postponed" as next input accum/state


instance ArrowChoice Circuit where
  left orig@(Circuit cir) = Circuit $ \ebd -> case ebd of
    Left b  -> let (cir1, c) = cir b
               in  (left cir1, Left c)
    Right d -> (left orig, Right d)


getWord :: StdGen -> Circuit () String
getWord gen = proc () -> do
  firstTime <- oneShot -< ()
  mbPicked <- if firstTime       -- NOT normal `if`, but syntax
                                 -- structure for 'ArrowChoice'!
              then do picked <- pickWord gen -< ()
                      returnA -< Just picked
              else returnA -< Nothing
  mbWord <- accum' Nothing mplus -< mbPicked
            -- where
            --   accum' :: s -> (a -> s -> s) -> Circuit a s
            --     spec to
            --   accum' :: m s -> (m s -> m s -> m s) -> Circuit (m s) (m s)
  returnA -< fromJust mbWord

  


-- * Hangman: Main program

