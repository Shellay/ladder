{-# LANGUAGE RankNTypes #-}
import Lens.Micro as L


data Point = Point { _x :: Double
                   , _y :: Double } deriving (Show)

data Line = Line { _start :: Point
                 , _end   :: Point } deriving (Show)

-- x :: Functor f => (Double -> f Double) -> Point -> f Point
x :: L.Lens' Point Double
x = L.lens getter setter
  where getter = _x
        setter = \pnt x -> pnt {_x = x}
y :: L.Lens' Point Double
y = L.lens _y $ \pnt y -> pnt {_y = y}


p0 = Point { _x = 0.0 , _y = 0.0 }
p1 = Point { _x = 0.1 , _y = 0.2 }
p2 = Point { _x = 0.3 , _y = 0.4 }

l12 = Line { _start = p1, _end = p2 }

-- start :: Functor f => (Point -> f Point) -> Line -> f Line
start :: L.Lens' Line Point
start = L.lens _start (\ln p -> ln { _start = p })

l02 = set start p0 l12


data Polygon = Polygon { _points :: [Point] } deriving (Show)

points :: L.Lens' Polygon [Point]
points = L.lens _points (\plg pnts -> plg { _points = pnts })


-- shift :: L.Lens' s Double -> (Double -> Double) -> s -> s
shift lns f = over lns f

plg1 = Polygon { _points = [p0, p1, p2] }
