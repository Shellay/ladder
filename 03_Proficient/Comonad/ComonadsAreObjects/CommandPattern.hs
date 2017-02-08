{-# LANGUAGE RankNTypes #-}
module CommandPattern where


-- * The Command Pattern

newtype Kelvin = Kelvin { getKelvin :: Double } deriving (Show)
newtype Celsius = Celsius { getCelsius :: Double } deriving (Show)

type Thermostat a = (Kelvin, Kelvin -> a)


kelvinToCelsius :: Kelvin -> Celsius
kelvinToCelsius (Kelvin t) = Celsius (t - 273.15)

initialThermostat :: (Kelvin, Kelvin -> Celsius)
initialThermostat = (Kelvin 298.15, kelvinToCelsius)

extract :: Thermostat a -> a
extract (t, f) = f t

up :: Thermostat a -> a
up (Kelvin t, f) = f (Kelvin (t + 1))

down :: Thermostat a -> a
down (Kelvin t, f) = f (Kelvin (t - 1))

toString :: (Kelvin, Kelvin -> Celsius) -> String
toString (k, f) = show (getCelsius $ f k) ++ "C" 


up' :: Thermostat a -> Thermostat a
up' (Kelvin t, f) = (Kelvin (t + 1), f)
down' :: Thermostat a -> Thermostat a
down' (Kelvin t, f) = (Kelvin (t - 1), f)


(.>) = flip ($)
infixl 5 .>

t1 = initialThermostat .> up'
t2 = t1 .> up'

-- needing stuff
-- preview :: Thermostat a -> b
-- extend preview :: Thermostat a -> (Kelvin, Kelvin -> b)

extend :: (Thermostat a -> b)
       -> (Thermostat a -> Thermostat b)
extend preview (k, f) = (k, \k' -> preview (k', f))


-- ** Ex
-- extract (extend preview thermostat) = preview thermostat

-- ** Ex
-- extend (\t' -> p2 (extend p1 t')) t = extend p2 (extend p1 t)

