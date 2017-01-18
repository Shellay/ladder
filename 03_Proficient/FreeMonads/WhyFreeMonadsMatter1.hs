{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE KindSignatures #-}
-- http://www.haskellforall.com/2012/06/you-could-have-invented-free-monads.html

-- * Interpreters
{-
• compile it to an executable,
• run it directly (i.e. the traditional sense of "interpret"),
• pretty print it,
• compress and archive it,
• or do nothing at all with it!
-}

-- abstraction as syntax tree

data Toy b next = Output b next -- node
                | Bell next     -- node
                | Done          -- leaf

prog1 =
  Output 'A' Done -- :: Toy Char (Toy a next)
prog2 =
  Bell (Output 'A' Done) -- :: Toy a (Toy Char (Toy b next))

-- Each program above has a new type.

-- Can wrap as many Toys into the same data type.

data Cheat f = MkCheat (f (Cheat f))


-- The fixed point of a functor
data Fix f = MkFix (f (Fix f))
  -- where f   :: * -> *
  --       Fix :: (* -> *) -> *

fprog1 =
  -- let next = Fix (Toy b)
  --     b    = Char
  --     f    = Toy b
  MkFix (Output 'A' (MkFix Done)) -- :: Fix (Toy Char)

fprog2 =
  MkFix (Bell $ MkFix (Output 'A' (MkFix Done))) -- :: Fix (Toy Char)

-- But they always needs a `Done`.


data FixE f e = Fix (f (FixE f e))
              -- where
              --   Fix :: f (FixE f e) -> FixE f e
              | Throw e

catch :: (Functor f) =>
  FixE f e1 -> (e1 -> FixE f e2) -> FixE f e2
catch (Throw e) g = g e
catch (Fix x) g   = Fix $ fmap (`catch` g) x
  -- where
  --   Fix x :: FixE f e1
  --   x     :: f (FixE f e1)
  --   g     :: e1 -> FixE f e2
  --
  --   `catch` g                :: FixE f e1 -> FixE f e2
  --   fmap (`catch` g)         :: f (FixE f e1) -> f (FixE f e2)
  --   fmap (`catch` g) x       :: f (FixE f e2)
  --   Fix $ fmap (`catch` g) x :: T_Fix $ f (FixE f e2) == FixE f e2

-- Make (Toy b) a functor to use `catch`
instance Functor (Toy b) where
  -- f :: Toy b n -> Toy b n
  fmap f (Output b nxt) = Output b (f nxt)
    -- where
    --   Output b nxt     :: Toy b next
    --   Output b (f nxt) :: Toy b next
  fmap f (Bell nxt)     = Bell (f nxt)
  fmap f (Done)         = Done

-- Catch it
data IncompleteException = IncompleteException

subroutine0 =
  Fix (Output 'A' (Throw IncompleteException))
  -- :: FixE (Toy Char) IncompleteException

program0 =
  subroutine0 `catch` \_ -> Fix (Bell (Fix Done))
  -- :: FixE (Toy Char) e



-- * Free Monads - Part 1

data Free f r = Free (f (Free f r))
              | Pure r

instance (Functor f) => Functor (Free f) where
  -- fmap :: (a -> b) -> Free f a -> Free f b
  fmap g (Pure a)  = Pure (g a)
  fmap g (Free fF) = Free $ fmap (fmap g) fF
    -- where
    --   fF     :: f (Free f a)
    --   g      :: a -> b
    --   fmap g :: Free f a -> Free f b
    --
    --   fmap (fmap g) fF :: f (Free f b)

instance (Functor f) => Applicative (Free f) where
  pure = Pure
  Pure f   <*> Pure a = Pure (f a)
  Free fFf <*> fFa    = Free $ (fmap (<*> fFa) fFf)
    -- where
    --   (<*>)              :: Free f (a -> b) -> Free f a -> Free f b
    --   (<*> fFa)          :: Free f (a -> b) -> Free f b
    --   fmap (<*> fFa)     :: f (Free f (a -> b)) -> f (Free f b)
    --   fFf                :: f (Free f (a -> b))
    --   fmap (<*> fFa) fFf :: f (Free f b)

instance (Functor f) => Monad (Free f) where
  Pure a   >>= g = g a
  Free fFa >>= g = Free $ fmap (>>= g) fFa
    -- where
    --   fFa              :: f (Free f a)
    --   g                :: a -> Free f b
    --   (>>=)            :: Free f a -> (a -> Free f b) -> Free f b
    --   (>>= g)          :: Free f a -> Free f b
    --   fmap (>>= g)     :: f (Free f a) -> f (Free f b)
    --   fmap (>>= g) fFa :: f (Free f b)

-- catch == (>>=)
-- Throw == Pure


-- smart constructors

output :: a -> Free (Toy a) ()
-- output x = Free $ Output x (Pure ())

bell :: Free (Toy a) ()
-- bell = Free $ Bell (Pure ())

done :: Free (Toy a) r
-- done = Free Done


liftF :: (Functor f) => f r -> Free f r
liftF cmd = Free $ fmap Pure cmd
  -- where
  --   Pure                 :: r -> Free f r
  --   fmap Pure            :: f r -> f (Free f r)
  --   fmap Pure cmd        :: f (Free f r)
  --   Free $ fmap Pure cmd :: Free f r

output a = liftF (Output a ())
bell     = liftF (Bell ())
done     = liftF Done


subroutine :: Free (Toy Char) ()
subroutine = output 'A'

program :: Free (Toy Char) r
program = do
  subroutine
  bell
  done
  -- These are NOT interpreted: they are PURE data.
  -- Only building a data type.


-- interprete by making string
showProgram :: (Show a, Show r) => Free (Toy a) r -> String
showProgram (Free (Output a x)) =
  "output " ++ show a ++ "\n" ++ showProgram x
showProgram (Free (Bell x)) =
  "bell\n" ++ showProgram x
showProgram (Free Done) =
  "done\n"
showProgram (Pure r) =
  "return " ++ show r ++ "\n"



-- * Concurrency

-- type Thread m = [m ()]

-- nesting subsequent action within previous one
data Thread m r = Atomic (m (Thread m r))
                | Return r

liftM :: Monad m => (a -> b) -> m a -> m b
liftM f = (>>= return . f)

atomic :: (Monad m) => m a -> Thread m a
atomic = Atomic . liftM Return
  -- where
  --   liftM Return :: m r -> m (Thread m r)

instance (Monad m) => Functor (Thread m) where
  fmap f (Return a)    = Return (f a)
  fmap f (Atomic mt) = Atomic $ mt >>= return . fmap f
  -- fmap f (Atomic mt) = Atomic $ do t <- mt
  --                                  return (fmap f t)
    -- where
    --   mt :: m (Thread m a)
    --   f  :: a -> b
    --   t  :: Thread m a
    --   fmap f t :: Thread m b
    --   return (fmap f t) :: m (Thread m b)
    --   Atomic (return (fmap f t)) :: Thread m b

instance (Monad m) => Applicative (Thread m) where
  pure = Return
  Return f <*> Return a = Return (f a)
  Return f <*> Atomic mtma = Atomic $ do tma <- mtma
                                         return $ Return f <*> tma -- :: m (Thread m a)
  Atomic mtmf <*> tma = Atomic $ do tmf <- mtmf
                                    return $ tmf <*> tma

instance (Monad m) => Monad (Thread m) where
  Return a    >>= f = f a
  Atomic mtma >>= f = Atomic $ liftM (>>= f) mtma
    -- where
    --   f   :: a -> Thread m b
    --   tma :: Thread m a
    --
    --   (>>= f)       :: Thread m a -> Thread m b
    --   liftM (>>= f) :: m (Thread m a) -> m (Thread m b)

thread1 :: Thread IO ()
thread1 = do
  atomic $ print 1
  atomic $ print 2

thread2 :: Thread IO ()
thread2 = do
  str <- atomic $ getLine
  atomic $ putStrLn str

-- interleaving
interleave :: (Monad m) =>
  Thread m r -> Thread m r -> Thread m r
interleave (Atomic m1) (Atomic m2) = do
  next1 <- atomic m1
  next2 <- atomic m2
  interleave next1 next2
interleave t1 (Return _) = t1
interleave (Return _) t2 = t2

runThread :: (Monad m) => Thread m r -> m r
runThread (Atomic m) = m >>= runThread
runThread (Return r) = return r

t12 = runThread (interleave thread1 thread2)

