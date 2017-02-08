{-# LANGUAGE Arrows #-}

module Main where

import Control.Arrow
-- import Control.Monad
-- import qualified Control.Category as Cat
-- import Data.List
import Data.Maybe
import System.Random

import Tutorial

attempts :: Int
attempts = 5

livesLeft :: Int -> String
livesLeft hung = "Lives: ["
                 ++ replicate (attempts - hung) '#'
                 ++ replicate hung ' '
                 ++ "]"

hangman :: StdGen -> Circuit String (Bool, [String])
hangman gen = proc usrInp -> do
  word <- getWord gen -< ()
  let letter = listToMaybe usrInp
  guessed <- updateGuess -< (word, letter)
  hung    <- updateHung  -< (word, letter)
  end     <- delayedEcho True -< not (word == guessed || hung >= attempts)
  let result = if word == guessed
               then [guessed, "You won!"]
               else if hung >= attempts
                    then [guessed, livesLeft hung, "You died!"]
                    else [guessed, livesLeft hung]
  returnA -< (end, result)

  where

    underscores :: String
    underscores = repeat '_'

    updateGuess :: Circuit (String, Maybe Char) String
    updateGuess = accum' underscores $ \(word, mLetter) guess ->
      case mLetter of
        Just l ->
          map (\(w, g) -> if w == l then w else g) (zip word guess)
        Nothing ->
          take (length word) guess
    
    updateHung :: Circuit (String, Maybe Char) Int
    updateHung = proc (word, letter) -> do
      total -< case letter of
                 Just l  -> if l `elem` word then 0 else 1
                 Nothing -> 0
                                         

main = return ()
