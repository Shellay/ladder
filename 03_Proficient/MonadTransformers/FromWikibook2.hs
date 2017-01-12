import Control.Applicative
import Control.Monad
-- import Control.Monad.Trans.State
-- import Control.Monad.Trans.State.Lazy
-- import Control.Monad.Trans.State.Strict
import Control.Monad.Trans.Class
import Control.Monad.Trans.Maybe

-- * A plethora of transformers

-- base monad
-- i.e. ‘Maybe’ for ‘MaybeT’

-- side monad
-- i.e. ‘IO’ for ‘(MaybeT IO) a’ (i.e. IO (Maybe a))


-- ** Type juggling

newtype (ListT m) a = ListT { runListT :: m [a] }

newtype (ExceptT e m) a = ExceptT { runExceptT :: m (Either e a) }

-- The relation between base monad and trans monad is not ruled.
{- e.g.

newtype Writer w a = Writer { runWriter :: (a, w) } 
newtype Reader r a = Reader { runReader :: r -> a }
newtype State s a = State { runState :: s -> (a, s) }
newtype Cont a r = Cont { runCont :: (a -> r) -> r }

Base Monad; Transformer; Original Type ("wrapped" by base); Combined Type ("wrapped" by transformer)

Writer; WriterT; (a, w)       ; m (a, w)
Reader; ReaderT; r -> a       ; r -> m a
State ; StateT ; s -> (a, s)  ; s -> m (a, s)
Cont  ; ContT  ; (a -> r) -> r; (a -> m r) -> m r

-}


-- * Lifting

{-
do x <- mx
   return (f x)

liftM f mx
-}

class MonadTrans' trans where
  lift' :: (Monad base) => base a -> (trans base) a

class (Monad m) => MonadIO' m where
  liftIO :: IO a -> m a


instance MonadTrans' MaybeT where
  lift' m = MaybeT $ (liftM Just) m


-- * Ex.

-- 1. ‘lift’ is defined seperately for each transformer since the
-- transformation is not universally defined with the same manner,
-- i.e. there are multiple type-consistent ways to transform monads
-- (combine more monads to one "big" monad).
--
-- OTOH, ‘liftM’ is like ‘fmap’, which lifts non-monadic values into
-- a singly monadic context.

-- 2. Identity

newtype Identity a = Identity { runIdentity :: a }

instance Functor Identity where
  fmap f (Identity a) = Identity (f a)

instance Applicative Identity where
  pure = Identity
  Identity f <*> Identity a = Identity (f a)

instance Monad Identity where
  Identity a >>= f = f a

newtype (IdentityT m) a = IdentityT { runIdentityT :: m a }

instance Monad m => Functor (IdentityT m) where
  fmap f (IdentityT ma) = IdentityT $ fmap f ma
instance Monad m => Applicative (IdentityT m) where
  pure = IdentityT . pure
  (IdentityT mf) <*> (IdentityT ma) = IdentityT $ mf <*> ma
instance Monad m => Monad (IdentityT m) where
  (IdentityT ma) >>= f = IdentityT $ do { a <- ma ; runIdentityT $ f a }

instance MonadTrans IdentityT where
  lift = IdentityT



-- * Implementing transformers

-- ** The ‘State’ transformer

newtype (StateT s m) a = StateT { runStateT :: (s -> m (a, s)) }

instance (Monad m) => Functor (StateT s m) where
  fmap f (StateT r) = StateT $ \s -> do (a, s1) <- r s
                                        return (f a, s1)
instance (Monad m) => Applicative (StateT s m) where
  pure a = StateT $ \s -> return (a, s)
  (StateT fr) <*> (StateT r) = StateT $ \s -> do (f, s1) <- fr s
                                                 (a, s2) <- r s1
                                                 return (f a, s2)
instance (Monad m) => Monad (StateT s m) where
  (StateT r) >>= f = StateT $ \s -> do (a, s1) <- r s
                                       -- f   :: a -> (StateT s m) b
                                       -- f a :: (StateT s m) b
                                       -- 
                                       -- runStateT (f a)   :: s -> m (a, s)
                                       -- runStateT (f a) s :: m (a, s)
                                       runStateT (f a) s1

  -- The ‘do’ block here is even more concise than the ‘let’ block in
  -- ‘State s’, which may mean that ‘StateT s m’ is more fundamental
  -- than ‘State s’.

-- In GHC 8.0 ‘State s’ is acturally implemented with transformer
-- ‘StateT s Identity’.
--
-- !! That is why now ‘State’ related things are now under "Trans",
-- i.e.  ‘Control.Monad.Trans.State’

-- instance (Monad m) => MonadState s (StateT s m) where
--   get   = StateT $ \s -> return (s, s)
--   put s = StateT $ \_ -> return ((), s)

instance (Monad m, Alternative m) => Alternative (StateT s m) where
  empty = StateT $ \s -> empty
  (StateT r1) <|> (StateT r2) = StateT $ \s -> r1 s <|> r2 s

instance (MonadPlus m) => MonadPlus (StateT s m) where
  mzero = StateT $ \s -> mzero
  (StateT r1) `mplus` (StateT r2) = StateT $ \s -> r1 s `mplus` r2 s

instance MonadTrans (StateT s) where
  lift ma = StateT $ \s -> do a <- ma
                              return (a, s)
