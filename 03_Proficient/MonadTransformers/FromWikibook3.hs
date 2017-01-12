{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}

import Control.Monad.Trans.State hiding (get, put)


-- * Implementing transformers

-- ** The ‘State’ transformer


class Monad m => MonadState s m | m -> s where
  get :: m s
  put :: s -> m ()



instance (Monad m) => MonadState s (StateT s m) where
  get   = StateT $ \s -> return (s, s)
  put s = StateT $ \_ -> return ((), s)


-- * Ex 1. Implement (state :: MonadState s m => (s -> (a, s)) -> m a)

state :: MonadState s m => (s -> (a, s)) -> m a
-- with m = StateT s n
--      (s -> (a, s)) -> (StateT s n) a
--      where
--        StateT s n a == StateT { \s -> n (a, s) }
-- state r = do s <- get
--              let (a, s1) = r s
--              put s1
--              return a
state r =
  get >>= \s ->
            let
              (a, s1) = r s
            in
              (put s1 >>= \() ->
                            return a)

-- The implementation of ‘state’ here is more general than using
-- ‘StateT’ directly, since (MonadState s m) is more general than
-- (StateT s m)!


-- * Ex 2. Are (MaybeT (State s)) and (StateT s Maybe) equivalent?

newtype MaybeT m a = MaybeT { runMaybeT :: m (Maybe a) }
-- then
-- MaybeT (State s) a = { State s (Maybe a) } == { s -> (Maybe a, s) }
--   which is an instance of (State s (m a))

newtype StateT' s m a = StateT' { runStateT' :: s -> m (a, s) }
-- then
-- StateT s Maybe a = { s -> Maybe (a, s) }
--   which is neither instance of (Maybe a) nor instance of (State s (m a)) 
