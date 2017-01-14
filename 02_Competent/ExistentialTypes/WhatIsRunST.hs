{-# LANGUAGE RankNTypes #-}

{-
A concise discuss here:  
http://stackoverflow.com/questions/12468622/how-does-the-st-monad-work
-}

-- *** Explanation *** --

-- * simulated "update-in-place"
-- * non-escapable

-- ST s a
-- runST :: forall a. (forall s. ST s a) -> a

-- * universal action in s


import Control.Monad.ST
import Data.STRef
import Control.Monad

sumST :: Num a => [a] -> a
sumST xs = runST $ do

  -- newSTRef :: a -> ST s (STRef s a)
  n <- newSTRef 0
  -- n :: Num a => ST s (STRef s a)

  -- forM_ :: (Foldable t, Monad m) => t a -> (a -> m b) -> m ()
  --   with t = []
  --        m = ST s
  forM_ xs $ \x -> do

    -- modifySTRef :: (STRef s a) -> (a -> a) -> ST s ()
    modifySTRef n (+x)

  -- readSTRef :: STRef s a -> ST s a
  --   with a = Num a => a
  readSTRef n

  -- runST :: (forall s. ST s a) -> a
  --   with a = Num a => a


-- * Any schematic a, b along the monad (ST s a)...(ST s b) has no
-- chance to be instantiated with any type expression containing (s)!


foldlST :: (a -> b -> a) -> a -> [b] -> a
foldlST f acc bs = runST $ do
  accRef <- newSTRef acc
  forM_ bs $ \b -> do
    a <- readSTRef accRef
    writeSTRef accRef (f a b)
  readSTRef accRef



-- Error
{-
foo =
  let
    -- runST :: (forall s. ST s a) -> a
    a = runST $ newSTRef (15 :: Int)
      -- where (newSTRef :: a -> ST s (STRef s a))
      -- 
      --       with newSTRef.a = Int
      --       newSTRef (15 :: Int) :: ST s (STRef s Int)
      --       
      --       with runST.a = STRef s Int
      --       runST (newSTRef ...) :: (STRef s Int)
      --       !! Type (s) is not quantified !!
      --       
    b = runST $ writeSTRef a 20
      -- where (writeSTRef :: (STRef s a) -> a -> ST a ())
    c = runST $ readSTRef a
  in
    b `seq` c
-}


-- Unification should be done with isolation of the type (s)
f :: (forall a. [a] -> b) -> Bool -> b
f g flag | flag      = g "abcd"
         | otherwise = g [1,2]

-- length   :: forall a. [a] -> Int
-- f length :: Bool -> Int
--   where no type argument is out of scope
-- OK.
r1 = f length

-- id   :: forall a. a -> a
-- f id :: Bool -> ?a?
--   with b = a
--   where type argument (a) is out of scope
-- FAIL.
-- r2 = f id


-- * Summarize
{-
runST :: forall a. (forall s. ST s a) -> a

!! The type (s) in (ST s a) is a phatom trick and no value of type (s)
is used. (s) is dedicated for context-protection through
type-checking.

!! When applying (runST) to a (ST s a) value, it must be guaranteed
that (a) is not any type having (s) as its component, i.e. (a) must be
irrelevant to (s) to be finally extracted.

!! During the monad-do computation within monad (ST s), the monad (ST
s) is "locked", and the operations (new, read, write, ...) are also
consistently locked. 

!! The stateful value from newSTRef, readSTRef, ... i.e. (STRef s t),
(ST s ()) etc always rely on (s), thus not be able to accessed out of
the (ST s) monad.

-}
