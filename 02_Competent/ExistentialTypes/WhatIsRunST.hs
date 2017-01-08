-- *** Explanation *** --

-- * update-in-place
-- * escapable

-- ST s a

-- * thread

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

-- * Any schematic a, b along the (ST s a)...(ST s b) monad has no
-- chance to be instantiated with any type expression containing s!


foldlST :: (a -> b -> a) -> a -> [b] -> a
foldlST f acc bs = runST $ do
  accRef <- newSTRef acc
  forM_ bs $ \b -> do
    a <- readSTRef accRef
    writeSTRef accRef (f a b)
  readSTRef accRef


-- Error
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
