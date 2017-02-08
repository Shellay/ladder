module IteratorPattern where

-- * The Iterator Pattern

data Iterator a = a :< (Iterator a) deriving (Show)
infixr 5 :<


initialHistory :: Iterator String
initialHistory = "" :< initialHistory -- loop

exampleHistory :: Iterator String
exampleHistory =
       "^D"
    :< "^C"
    :< "eat flaming death"
    :< "hello?"
    :< "bye"
    :< "exit"
    :< "quit"
    :< "?"
    :< "help"
    :< "ed"
    :< initialHistory

extract :: Iterator a -> a
extract (cmd :< _) = cmd

next :: Iterator a -> Iterator a
next (_ :< itr) = itr

next1 :: Iterator a -> a
next1 (_ :< (cmd :< _)) = cmd

next2 :: Iterator a -> a
next2 itr = extract (next (next itr))


-- retrieval :: Iterator a -> a
-- extend retrieval :: Iterator a -> Iterator a

extend :: (Iterator a ->          a)
       -> (Iterator a -> Iterator a)
extend rt (a :< as) = rt (a :< as) :< extend rt as


-- ** Ex (extend next1 =?= next)

{-
extract (extend retrieval iterator) =?= retrieval iterator

WHERE
LHS
==
extract (extend retrieval (a :< as))
==
extract (retrieval (a :< as) :< extend retrieval as)
==
retrieval (a :< as)
==
RHS

OK.
-}

{-
extend extract (a :< as) 
==
(extract (a :< as)) :< (extend extract as)
==
a :< (extend extract as)

OTOH

id (a :< as)
==
a :< (id as)

MATCHING

id (a :< as) = extend extract (a :< as)
a :< (id as) = a :< (extend extract as)

I.E.

id == extend extract
-}


-- ** Ex
-- extend (\j -> r2 (extend r1 j)) i =?= extend r2 (extend r1 i)
{-
LHS
==
extend (\j -> r2 (extend r1 j)) (a:<as)
==
extend {r2 (extend r1 ?)} (a:<as)
==
r2 (extend r1 (a :< as)) :< extend {r2 (extend r1 ?)} as
==
r2 (r1 (a:<as) :< extend r1 as) :< extend {r2 (extend r1 ?)} as
==
b :< LHS'

RHS
==
extend r2 (extend r1 (a:<as))
==
extend r2 (r1 (a:<as) :< extend r1 as)
==
r2 (r1 (a:<as) :< extend r1 as) :< extend r2 (extend r1 as)
==
b :< RHS'

I.E.

LHS' == RHS' 
<==> 
LHS == RHS

OK.
-}

  
