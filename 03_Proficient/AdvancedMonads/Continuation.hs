main :: IO ()
main = return ()

newtype State s a = State { runState :: s -> (a, s) }

newtype Cont r a  = Cont  { runCont  :: (a -> r) -> r}

instance Functor (Cont r) where
  fmap f (Cont arr) = Cont $ \br ->
                               arr (\a -> br (f a))

instance Applicative (Cont r) where
  pure a = Cont $ \ar -> ar a
  Cont frr <*> Cont arr = Cont $ \br ->
                                   frr (\f ->
                                          arr (\a ->
                                                 br (f a)))

instance Monad (Cont r) where
  Cont arr >>= g = Cont $ \br ->
                            arr (\a -> runCont (g a) br)

{-
callCC :: ((a -> Cont r b) -> Cont r a) -> Cont r a
callCC t = Cont $ \ar ->
                    runCont (t (\a -> Cont $ \br -> ar a)) ar
                    -- where
                    --   t is a continuation maker preparing some return value
                    --   (runCont ... ar) is the cheater component for t
-}

when :: (Monad m) => Bool -> m () -> m ()
when True m = m
when False _ = return ()

divExcpt :: Int -> Int -> (String -> Cont r Int) -> Cont r Int
divExcpt x y handler =          --
  callCC $ \ok ->               -- ok  :: (Int -> Cont r Int) ::: the OK cont maker
  do                            -- err :: String
    msg <- callCC $ \notOk ->   -- notOk :: (String -> Cont r Int)
                      case y of
                        0 -> (notOk "Dividing by 0")
                        _ -> ok $ x `div` y
    handler msg                 -- handler can be (error :: String -> a)


-- The `callCC` property is specified by MonadCont
class Monad m => MonadCont m where
  callCC :: ((a -> m b) -> m a) -> m a

instance MonadCont (Cont r) where
  -- callCC :: ((a -> Cont r b) -> Cont r a) -> Cont r a
  callCC t = Cont $ \ar ->
                      runCont (t (\a -> Cont $ \br -> ar a)) ar


-- General `try-handle` logic.
tryCont :: MonadCont m =>
  ((e -> m a) -> m a) -> (e -> m a) -> m a
tryCont mkr hdl =               -- (mkr) makes a continuation with a
                                -- associated handler!
  callCC $ \ok ->               -- ok    :: a -> m a
  do e <-
       callCC $ \notOk ->       -- notOk :: e -> m a
       do
         a <- mkr notOk         -- notOk is passed further!
         ok a
     hdl e
