module Vector where

class VectorT v where
    magn    :: (Floating a) => v a -> a
    angle   :: (Floating a) => v a -> v a -> a
    between :: (Eq a, Floating a) => v a -> v a -> v a -> Bool
    (/\)    :: (Floating a) => v a -> v a -> v a
    (<.>)   :: (Num a) => v a -> v a -> a
    (|*)    :: (Num a) => a -> v a -> v a
    (*|)    :: (Num a) => v a -> a -> v a
    neg     :: (Num a) => v a -> v a

newtype Vector a = Vector { list :: [a] } deriving(Show)
instance VectorT Vector where
    u <.> v = sum $ zipWith (*) (list u) (list v)
    magn = sqrt . sum . map (**2) . list
    angle u v = acos ((u <.> v) / (magn u * magn v))
    between u v w = (angle u v) + (angle u w) == (angle v w)
    u /\ v = Vector { list = [x, y, z] }
        where x = (list u)!!1 * (list v)!!2 - (list u)!!2 * (list v)!!1
              y = (list u)!!2 * (list v)!!0 - (list u)!!0 * (list v)!!2
              z = (list u)!!0 * (list v)!!1 - (list u)!!1 * (list v)!!0
    k |* v = Vector { list = map (*k) (list v) }
    v *| k = k |* v
    neg v = (-1) |* v
