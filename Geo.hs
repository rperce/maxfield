module Geo where
import Vector
type Latitude = Double
type Longitude = Double
data Geodesic = Geodesic Latitude Longitude deriving Show
data GreatCircleSegment = GreatCircleSegment { start :: Vector Double, end :: Vector Double } deriving Show
type GCS = GreatCircleSegment

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
containsPoint :: Triangle -> Geodesic -> Bool
containsPoint (Triangle a b c) d = (sameSide a b c d) && (sameSide a c b d) && (sameSide b c a d)
    where sameSide a b c d = (dotNorm (side a b) c) == (dotNorm (side a b) d)
          dotNorm g v = normal g <.> fromGeo v
          side a b    = GreatCircleSegment { start = fromGeo a, end = fromGeo b }
