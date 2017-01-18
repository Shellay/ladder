newtype State s a = State { runState :: s -> (a, s) }

instance Functor (State s) where
  fmap f (State ra) = State $
    \s -> let (a, s1) = ra s
          in  (f a, s1)

instance Applicative (State s) where
  pure a = State $ \s -> (a, s)
  State fra <*> State ra = State $
    \s -> let (f, s1) = fra s
              (a, s2) = ra s1
          in  (f a, s2)
    
instance Monad (State s) where
  State ra >>= f = State $
    \s -> let (a, s1) = ra s
          in  runState (f a) s1


-- (StateT) is totally independent of the naive (State).

newtype StateT s m a = StateT { runStateT :: s -> m (a, s) }


-- fmap :: (a -> b) -> f a -> f b
-- (f = (c ->)) => fmap :: (a -> b) -> (c -> a) -> (c -> b)

-- instance Functor f => Functor (StateT s f) where
--   fmap g (StateT r) = StateT $ \s -> let h = \(a, s) -> (g a, s)
--                                      in  fmap h (r s)

instance Monad m => Functor (StateT s m) where
  fmap g (StateT r) = StateT $ \s -> do (a, s1) <- r s
                                        return (g a, s1)

instance Monad m => Applicative (StateT s m) where
  pure a = StateT $ \s -> return (a, s)
  (StateT fr) <*> (StateT r) = StateT $ \s -> do (f, s1) <- fr s
                                                 (a, s2) <- r s
                                                 return (f a, s2)

instance Monad m => Monad (StateT s m) where
  (StateT r) >>= f = StateT $ \s -> do (a, s1) <- r s
                                       (b, s2) <- runStateT (f a) s1
                                       return (b, s2)

class MonadTrans t where
  lift :: Monad m => m a -> t m a

instance MonadTrans (StateT s) where
  lift ma = StateT $ \s -> do a <- ma
                              return (a, s)
