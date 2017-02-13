module StaticDynamicParsers where

import Control.Arrow
import qualified Control.Category as Cat
import Data.List (union)

-- ** Static and dynamic parsers

data Parser s a b =
  P (StaticParser s) (DynamicParser s a b)

data StaticParser s = SP Bool [s]

newtype DynamicParser s a b = DP ((a, [s]) -> (b, [s]))
  -- underlying (a -> b)
  -- ([s] -> [s])


spChar :: Char -> StaticParser Char
spChar c = SP False [c]

dpChar :: Char -> DynamicParser Char Char Char
dpChar c = DP $ \(_, x:xs) -> (x, xs)
-- | Testing the first input char is done by static parser.

charA :: Char -> Parser Char Char Char
charA c = P (SP False [c]) (DP $ \(_, x:xs) -> (x, xs))
  

-- ** Bringing the arrow combinator in

instance Eq s => Cat.Category (Parser s) where

  id = P (SP True []) (DP $ \(b, s) -> (b, s))

  P (SP e1 start1) (DP p2) . P (SP e2 start2) (DP p1) =
    P (SP
       (e1 && e2)
       (if not e1 then start1 else start1 `union` start2)) (DP $ p2 . p1)
    

instance Eq s => Arrow (Parser s) where

  -- arr :: (a -> b) -> a `Parser s` b
  arr f = P (SP True []) (DP $ \(b, s) -> (f b, s))

  -- first :: (a `Parser s` b) -> ((a, z) `Parser s` (b, z))
  first (P sp (DP p)) = P sp (DP $ \((a, z), s) ->
                                     let (b, s1) = p (a, s)
                                     in  ((b, z), s1))
