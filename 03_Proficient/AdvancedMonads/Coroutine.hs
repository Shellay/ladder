{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}

import Control.Applicative
import Control.Monad.Trans.Class
import Control.Monad.Trans.Cont hiding (callCC)
import Control.Monad.Trans.State
import Control.Monad.IO.Class


class MonadCont c where
  callCC :: ((a -> c b) -> c a) -> c a

instance MonadCont (ContT r m) where
  callCC t = ContT $ \ar ->     -- ar :: a -> m r
    runContT (t (\a -> ContT $ \br -> ar a)) ar


-- `CoroutineT` is specialized `ContT` where inner monad is a `StateT`
-- with a list of `Coroutine`s as its state.
newtype CoroutineT r m a = CoroutineT {
  runCoroutineT ::
      ContT r (StateT [CoroutineT r m ()] m) a
  } deriving (Functor, Applicative, Monad, MonadIO, MonadCont)

-- Recursive synonym is not allowed:
-- type CoroutineT' r m a = ContT r (StateT [CoroutineT' r m ()] m) a


-- Used to manipulte the coroutine queue

getCCs :: Monad m => CoroutineT r m [CoroutineT r m ()]
getCCs = CoroutineT $ lift get
         -- where
         --   get      :: Monad m      => StateT s m s
         --   lift     :: MonadTrans t => n a -> t n a
         --
         --   with
         --     n = StateT s m :: * -> *
         --     a = s          :: *
         --     t              :: (* -> *) -> * -> *
         -- 
         --   lift get :: t (StateT s m) s
         --
         --   with
         --     t = ContT      :: k -> (k -> *) -> * -> *
         --     lift get       :: ContT r (StateT s m) s
         --
         --   lift get :: ContT r (StateT s m) s
         --
         --   with
         --     CoroutineT     :: ContT r (State [CrtT r m ()] m) a -> CoroutineT r m a
         --     s = [CrtT r m ()]
         --     lift get       :: ContT r (StateT [CrtT r m ()] m) [CrtT r m ()]
         --     a = s
         --     
         --   getCCs   :: CoroutineT r m [CoroutineT r m ()]

putCCs :: Monad m => [CoroutineT r m ()] -> CoroutineT r m ()
-- putCCs = CoroutineT . lift . put
putCCs ccs = CoroutineT $ lift (put ccs)
             -- where
             --   Monad m
             --   put :: s -> StateT s m ()
             --   
             --   with
             --     s = [CrtT r m ()]
             --     put :: [CrtT r m ()] -> StateT [CrtT r m ()] m ()
             --
             --   put ccs :: StateT [CrtT r m ()] m ()
             --   lift    :: n a -> t n a
             --   t       :: (* -> *) -> * -> *
             --
             --   with
             --     n = StateT [CrtT r m ()] m :: * -> *
             --     a = ()                     :: *
             --     ContT                      :: k -> (k -> *) -> * -> *
             --     t = ContT r                :: (k -> *) -> * -> *
             --
             --   lift    :: (StateT s m) () -> (ContT r) (StateT s m) ()
             --   
             --   lift (put ccs) :: ContT r (State [CrtT r m ()]) ()


-- Pop and push coroutines.
dequeue :: Monad m => CoroutineT r m ()
dequeue = do
  ccs <- getCCs
  case ccs of
    []     -> return ()
    (c:cs) -> do putCCs cs
                 c

enqueue :: Monad m => CoroutineT r m () -> CoroutineT r m ()
enqueue c = do
  ccs <- getCCs
  putCCs (ccs ++ [c])


-- The interface.
yield :: Monad m => CoroutineT r m ()
yield = callCC $ \k ->          -- k :: (a -> CoroutineT r m b) -> CoroutineT r m a
  do
    enqueue (k ())
    dequeue

fork :: Monad m => CoroutineT r m () -> CoroutineT r m ()
fork c = callCC $ \k ->
  do
    enqueue (k ())
    c
    dequeue


-- Exhaust
exhaust :: Monad m => CoroutineT r m ()
exhaust = do
  exhausted <- null <$> getCCs
  if exhausted
    then return ()
    else do yield
            exhaust

