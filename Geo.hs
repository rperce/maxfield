module Geo where
import Vector
type Latitude = Double
type Longitude = Double
data Geodesic = Geodesic Latitude Longitude deriving Show
data GreatCircleSegment = GreatCircleSegment { start :: Vector Double, end :: Vector Double } deriving Show
type GCS = GreatCircleSegment

instance Eq Geodesic where
    (Geodesic a b) == (Geodesic c d) = (a == c) && (b == d)
fromGeo :: Geodesic -> Vector Double
normal :: GreatCircleSegment -> Vector Double
intersects :: GreatCircleSegment -> GreatCircleSegment -> Bool
gcs :: Geodesic -> Geodesic -> GreatCircleSegment

fromGeo (Geodesic lat long) = Vector { list = [x, x * tan(lat), x * tan(long)] }
    where x = 1.0 / (magn $ Vector { list = [tan(lat), tan(long), 1] })
gcs s e = GreatCircleSegment { start = fromGeo s, end = fromGeo e }
normal g = (start g) /\ (end g)
intersects g h = (twixt cross g) && (twixt cross h)
    where   twixt v c = (between v       (start c) (end c)) ||
                        (between (neg v) (start c) (end c))
            cross = (normal g) /\ (normal h)

data Triangle = Triangle Geodesic Geodesic Geodesic deriving Show
instance Ord Triangle where
    t1 `compare` t2 = (area t1) `compare` (area t2)

instance Eq Triangle where
    t1 == t2 = (area t1) == (area t2)

containsPoint :: Triangle -> Geodesic -> Bool
containsPoint (Triangle a b c) d = notin d && (sameSide a b c d) && (sameSide a c b d) && (sameSide b c a d)
    where sameSide a b c d = (signum $ dotNorm (side a b) c) == (signum $ dotNorm (side a b) d)
          dotNorm g v = normal g <.> fromGeo v
          side a b    = GreatCircleSegment { start = fromGeo a, end = fromGeo b }
          notin d = a /= d && b /= d && c /= d

area :: Triangle -> Double
area (Triangle a b c) = (ang a b b c) + (ang b c c a) + (ang c a a b) - pi
    where ang a b c d = angle (norm a b) (norm c d)
          norm a b    = normal $ GreatCircleSegment { start = fromGeo a, end = fromGeo b}
