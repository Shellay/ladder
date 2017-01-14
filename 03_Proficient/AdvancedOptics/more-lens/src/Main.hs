module Main where

import Playground
import LearnMoreLens

main :: IO ()
main = do
  putStrLn "hello world"

greet :: String -> String
greet name = "Hello, " ++ name ++ "!"

newtype MaybeT m a = MaybeT { runMaybeT :: m (Maybe a) }
