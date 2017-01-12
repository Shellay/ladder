import Data.Char (ord, chr)
import Control.Applicative
import Control.Monad
import Control.Monad.Trans.Class


-- * Passphrase validation

getPassphrase0 :: IO (Maybe String)
getPassphrase0 =
  do
    s <- getLine
    if isValid s
      then return $ Just s
      else return Nothing

isValid :: String -> Bool
isValid = allTrue . (conds <*>) . pure
  where
    conds = [(>= 8) . length,
             any isAlpha,
             any isNumber,
             any isPunctuation]
    allTrue = all id
    isAlpha = \x -> (x `elem` ['A'..'Z']) || (x `elem` ['a'..'z'])
    isNumber = (`elem` ['0'..'9'])
    isPunctuation = (`elem` "~!@#$%^&*()~,./<>?;':")

askPassphrase0 :: IO ()
askPassphrase0 = do
  putStrLn "Insert new passphrase:"
  s <- getPassphrase0
  case s of
    Just s  -> putStrLn "Storing ..."
    Nothing -> putStrLn "Invalid!"


-- main :: IO ()
-- main = ask
--   where ask = askPassphrase


-- * A simple monad transformer (MaybeT)

newtype MaybeT m a = MaybeT { runMaybeT :: m (Maybe a) }

instance Monad m => Functor (MaybeT m) where
  fmap f (MaybeT mma) = MaybeT $
    -- mma >>= \ma ->
    do
      ma <- mma
      case ma of
        Nothing -> return Nothing
        Just a  -> return $ Just (f a)

instance Monad m => Applicative (MaybeT m) where
  pure = MaybeT . return . Just
  (MaybeT mmf) <*> (MaybeT mma) = MaybeT $ do
    mf <- mmf
    ma <- mma
    return $ mf <*> ma

instance Monad m => Monad (MaybeT m) where
  (MaybeT mma) >>= f = MaybeT $ do
    ma <- mma
    case ma of
      Nothing -> return Nothing
      Just a  -> runMaybeT (f a) -- flatMap


-- more classes

instance Monad m => Alternative (MaybeT m) where
  empty = MaybeT $ return Nothing
  (MaybeT mmx) <|> (MaybeT mmy) = MaybeT $ do
    mx <- mmx
    case mx of
      Just x  -> mmx
      Nothing -> mmy

instance Monad m => MonadPlus (MaybeT m) where
  mzero = empty
  mplus = (<|>)

instance MonadTrans MaybeT where

  -- lift :: (MonadTrans t, Monad m) => m a -> t m a

  -- lift :: (t = MaybeT, Monad m) =>
  --          m a -> MaybeT m a
  --            where (MaybeT m a) = MaybeT { m (Maybe a) }

  -- lift = MaybeT . (liftM Just)

  -- lift ma = MaybeT $ do { a <- ma ; return (Just a) }
  lift = MaybeT . (fmap Just)


-- ** Application to the example

getPassphrase :: (MaybeT IO) String
getPassphrase = do
  -- (MaybeT IO) s == IO (Maybe s)
  s <- lift getLine
  guard (isValid s)
  return s

askPassphrase :: (MaybeT IO) ()
askPassphrase = do
  lift $ putStrLn "Give new passphrase:" -- lift (IO ()) = (MaybeT IO) ()
  s <- getPassphrase                     -- s <- ((MaybeT IO) String) ; s :: String
  lift $ putStrLn "Storing..."

askPassphraseMoreTimes :: (MaybeT IO) ()
askPassphraseMoreTimes = do
  lift $ putStrLn "Give new passphrase:"
  s <- msum $ repeat getPassphrase
  lift $ putStrLn "Storing..."


main :: IO ()
main = do
  runMaybeT $
    -- askPassphrase :: (MaybeT IO) ()
    askPassphrase
  -- runMaybeT askPassphrase :: IO (Maybe ())
  return ()
