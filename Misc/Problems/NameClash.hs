{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DuplicateRecordFields #-}

{- * In haskell, declaring record induces record field selector in
global scope, which may be annoying -}

data Human = Human { name :: String }

-- (name :: Human -> String) is induced into the global scope

-- Reusing the ‘name’ field will fail. But -XDuplicateRecordFields can
-- fix this.
data Dog = Dog { name :: String }

h1 = Human { name = "John" }
h2 = Human { name = "Sarah" }

d1 = Dog { name = "Doodie" }

-- Even with -XDuplicateRecordFields, accessing cannot be done:
-- h1n = name h1
--
-- Neither with signature associated:
-- h1n = (name :: Human -> String) h1


-- * Solutions
-- 
-- ++ Avoid same words for naming.
-- 
-- ++ Use of the same name in different modules
--   - and use *qualified* import!
--
-- ++ typeclass (HasName) 


data Person = Person { _name :: String }
data Cat = Cat { _name :: String }

class HasName d where
  name :: d -> String

instance HasName Person where
  name = _name

instance HasName Cat where
  name = _name

p1 = Person { _name = "Lucas" }

c1 = Cat { _name = "Kathie" }


-- ** Suppose we need a generic global ‘run’ method

class HasRun a t | t -> a where
  run :: t -> a

newtype State s a = State { _run :: s -> (a, s) }
newtype Identity a = Identity { _run :: a }

instance HasRun (s -> (a, s)) (State s a) where
  run = _run

instance HasRun a (Identity a) where
  run = _run

s1 = State $ \s -> (1, s)
i1 = Identity $ 8



-- * More extensive solutions in current time
--
-- ‘Lens’



-- * Proposals
{-
OverloadedRecordFields
-}
