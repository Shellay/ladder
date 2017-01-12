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


data Child = Child { nickName :: String, age :: Int }
