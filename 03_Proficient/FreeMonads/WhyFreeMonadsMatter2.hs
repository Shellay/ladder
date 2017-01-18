{-# LANGUAGE DeriveFunctor #-}
-- * Free Monads - Part 2

data Free f r = Free (f (Free f r))
              | Pure r

data List a   = Cons a (List a)
              | Nil


-- free monads like a list of functors

type List' a = Free ((,) a) ()
  -- where
  --   List' a ==
  --   with
  --     f = ((,) a)
  --       = * -> (a,*)
  --     r = ()
  --     Free f a = List' a
  --   in
  --     Free (f (Free f r))
  --   Free ((,) a) () ==
  --   Free (a, List' a) | Pure () ~=
  --   Free a (List' a)  | Pure ()


-- A list is a special case of a free monad.
--   where
--     join   ~= (++)
--     return ~= []

singleton :: a -> List a
singleton x = Cons x Nil

-- liftF = Free . fmap Pure

merge (x:xs) (y:ys) = x:y:merge xs ys
merge xs [] = xs
merge [] ys = ys

{-
interleave (Atomic m1) (Atomic m2) = do
  n1 <- liftF m1
  n2 <- liftF m2
  interleave n1 n2
-}


-- typeclasses

instance Functor f => Functor (Free f) where
  fmap g (Free fFfa) = Free $ fmap (fmap g) fFfa
                       -- where
                       --   g             :: a -> b
                       --   fmap g        :: Free f a -> Free f b
                       --   fmap (fmap g) :: f (Free f a) -> f (Free f b)

instance Functor f => Applicative (Free f) where
  pure = Pure
  Pure g    <*> x = fmap g x
  Free fFfg <*> x = Free $ fmap (<*> x) fFfg
                    -- where
                    --   (<*>)             :: Free f (a -> b) -> Free f a -> Free f b
                    --   (<*> x)           :: Free f (a -> b) -> Free f b
                    --   fmap (<*> x)      :: f (Free f (a -> b)) -> f (Free f b)
                    --   fmap (<*> x) fFfg :: f (Free f b)

instance Functor f => Monad (Free f) where
  Pure a    >>= h = h a
  Free fFfa >>= h = Free $ fmap (>>= h) fFfa


-- * Interpreters - Revisited

-- mainGame :: [Response] -> [Request]

data Direction = Forward | Backward deriving (Show)


-- data Request = Look Direction
--              | ReadLine
--              | Fire Direction
--              | WriteLine String

-- data Response = Image Picture   -- resp for Look
--               | ChatLine String -- resp for Read
--               | Succeeded Bool  -- resp for Write

data Picture = Picture deriving (Show)
data Image = Image [Int] deriving (Show)

-- no clear coupling between requests and responses

-- merging

data Interaction next
  = Look Direction (Image -> next)
  | Fire Direction next
  | ReadLine (String -> next)
  | WriteLine String (Bool -> next)
  deriving (Functor)

-- 

{- can use DeriveFunctor to do these:

instance Functor Interaction where
  fmap f (Look d g) = Look d (f . g)
  fmap f (Fire d n) = Fire d (f n)
  fmap f (ReadLine g) = ReadLine (f . g)
  fmap f (WriteLine s p) = WriteLine s (f . p)
-}


type Program = Free Interaction

easyToAnger :: Program r
easyToAnger = Free $ ReadLine $
  \s ->
    case s of
      "No" -> Free $ Fire Forward $
              Free $ WriteLine "Take that!" (\_ -> easyToAnger)
      _    -> easyToAnger


-- Use IO to simulate Game.
type Game = IO

-- Semantic interpretation from language (Program r) into language
-- (Game r).
interpret :: Program r -> Game r
interpret prog = case prog of
  Free (Look d g) -> do
    img <- collectImage d
    interpret (g img)
  Free (Fire d n) -> do
    sendBullet d
    interpret n
  Free (ReadLine g) -> do
    s <- getChatLine
    interpret (g s)
  Free (WriteLine s g) -> do
    putChatLine s
    interpret (g True)
  Pure r ->
    return r
  where
    collectImage d = return $ Image [1,2,3]
    sendBullet d   = return $ "Shooting " ++ show d
    getChatLine    = return $ "Chatting info"
    putChatLine _  = return ()
    

liftF :: Functor f => f a -> Free f a
liftF fa = Free $ fmap Pure fa
  -- where
  --   Pure :: a -> Free f a
  --   fmap Pure :: f a -> f (Free f a)

look d = liftF (Look d id)
fire d = liftF (Fire d ())
readLine = liftF (ReadLine id)
writeLine s = liftF (WriteLine s id)


-- uninterpreted program

when :: Monad m => Bool -> m () -> m ()
when True m = m
when False _ = return ()

forever :: Monad m => m a -> m a
forever m = do _ <- m
               forever m

easyToAnger0 :: a -> Program a
easyToAnger0 a = forever $ do
  str <- readLine
  when (str == "No") $ do
    fire Forward
    _ <- writeLine "Take that!"
    return ()
  return a

foo b = do
  print 3
  when b $
    print "wa"
  print 9



-- * Free Monads - Part 3

-- The free monad is the interpreter's best friend.

-- Formulation with the most flexibility.
-- Purely syntactic.

-- "Free-est" objects that still forms a monad.

