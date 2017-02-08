import Control.Monad.Trans.State

-- * Unfold as generator (corecursive structure)

-- cf. "Functional Programming in Scala"

unfold :: s -> (s -> Maybe (a, s)) -> [a]
unfold s st = case (st s) of
                Nothing      -> []
                Just (a, s1) -> a : unfold s1 st

mapU :: (a -> b) -> [a] -> [b]
mapU f as = unfold as g
  where
    g []     = Nothing
    g (a:as) = Just (f a, as)


-- unfoldM :: Traversable t => s -> StateT s m a -> t a
-- unfoldM s st = do a <- runStateT st s
                  
