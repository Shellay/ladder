module BuilderPattern where

-- * The Builder Pattern

type Flag = String

data Config = MkConfig [Flag] deriving (Show)

-- if not exposing 'MkConfig'
configBuilder :: [Flag] -> Config
configBuilder = MkConfig
  
defaultBuilder :: [Flag] -> Config
defaultBuilder flags = MkConfig ("-Wall":flags)

-- someBuilder :: [Flag] -> Config

profile :: ([Flag] -> Config) -> Config
profile builder = builder ["-prof", "-auto-all"] 
  
goFaster :: ([Flag] -> Config) -> Config
goFaster builder = builder ["-O2"]


-- How to chain 'profile' and 'goFaster'?

{- In OO-context
@
val builder = new DefaultConfig
builder.profile()
builder.goFaster()
val config = builder.extract
@
-}

-- sort like building continuations

profile' :: ([Flag] -> Config) -> ([Flag] -> Config)
profile' builder = \flags -> builder $ ["-prof", "-auto-all"] ++ flags

goFaster' :: ([Flag] -> Config) -> ([Flag] -> Config)
goFaster' builder = \flags -> builder $ ["-O2"] ++ flags

extract :: ([Flag] -> Config) -> Config
extract builder = builder []
  

-- sugar
(.>) :: a -> (a -> b) -> b
x .> f = f x
infixl 1 .>

b1 = defaultBuilder .> profile'
b2 = b1 .> goFaster'
cfg1 = b2 .> extract
  

-- Do not want to rewrite 'profile' and 'goFaster'...
-- i.e. from '([Flag] -> Config) -> Config'
--      to   '([Flag] -> Config) -> ([Flag] -> Config)'

type Builder  = [Flag]  -> Config
type Setter   = Builder -> Config
type Extender = Builder -> Builder

appender :: [Flag] -> Builder -> Builder
appender flags builder = \fs -> builder (fs ++ flags)

profile1 :: Builder -> Builder
profile1 = appender ["-prof", "-auto-all"]

goFaster1 :: Builder -> Builder
goFaster1 = appender ["-O2"]



-- extend :: (([Flag] -> Config) -> (          Config))
--        ->  ([Flag] -> Config) -> ([Flag] -> Config)
extend' :: (Builder -> Config) -> (Builder -> Builder)
extend' setter builder =
  \fs2 -> setter (\fs1 -> builder $ fs1 ++ fs2)

cfg2 = defaultBuilder .> (extend' profile) .> (extend' goFaster) .> extract


-- | Law: extract (extend setter builder) == setter builder


-- ** Ex 1
{-
extend extract builder =?= builder

WHERE
extend extract builder 
==
\fs2 -> extract (\fs1 -> builder (fs1 ++ fs2)) 
==
\fs2 -> builder ([] ++ fs2)
==
\fs2 -> builder fs2
==
builder

OK.
-}

-- ** Ex 2
{-
s1, s2 :: Builder -> Config     -- Setter
b , b' :: Builder               

extend (\b' -> s2 (extend s1 b')) b =?= extend s2 (extend s1 b)

WHERE

LHS
with setter  = \b' -> s2 (extend s1 b')
     builder = b
==
extend (\b' -> s2 (extend s1 b')) b
==
\fs2 -> (\b' -> s2 (extend s1 b')) (\fs1 -> b $ fs1 ++ fs2)
==
with b' = (\fs1 -> b $ fs1 ++ fs2)
\fs2 -> s2 (extend s1 (\fs1 -> b $ fs1 ++ fs2)) 
==
\fs2 -> s2 (\fs4 -> s1 $ \fs3 -> (\fs1 -> b $ fs1 ++ fs2) (fs3 ++ fs4)) 
=β(fs1)=
\fs2 -> s2 (\fs4 -> s1 $ \fs3 -> b $ (fs3 ++ fs4) ++ fs2) 
=α=
\z -> s2 (\y -> s1 $ \x -> b $ (x ++ y) ++ z) 
=ASSOC(++)=
\z -> s2 (\y -> s1 $ \x -> b $ x ++ y ++ z) 

RHS
='extend'=
\fs2 -> s2 (\fs1 -> (extend s1 b) $ fs1 ++ fs2)
='extend'=
\fs2 -> s2 (\fs1 -> (\fs4 -> s1 (\fs3 -> b $ fs3 ++ fs4)) $ fs1 ++ fs2)
=β(fs4)=
\fs2 -> s2 (\fs1 -> s1 $ \fs3 -> b $ fs3 ++ (fs1 ++ fs2))
=α=
\z -> s2 (\y -> s1 $ \x -> b $ x ++ (y ++ z))
=ASSOC(++)=
\z -> s2 (\y -> s1 $ \x -> b $ x ++ y ++ z) 
==
LHS

OK.
-}


