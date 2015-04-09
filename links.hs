import qualified Geo
import System.Environment (getArgs)
import Text.Regex.Posix
import Data.List

extractCoords :: String -> [String]
extractCoords line = (tail . concat) (line =~ "pll=(.+)\\|(.+)" :: [[String]])

toGeodesic :: String -> [Geo.Geodesic]
toGeodesic s = zipWith (Geo.Geodesic) lat long
    where   lat     = map (\x -> read ((head . tail . concat) x) :: Double) comma
            long    = map (\x -> read ((last . concat) x) :: Double) comma
            comma   = map (\x -> x =~ "(.+),(.+)" :: [[String]]) coords
            coords  = map (head . extractCoords) $ lines s

triangles :: [Geo.Geodesic] -> [Geo.Triangle]
triangles list = map toTri (triples list)
    where   toTri [x, y, z] = Geo.Triangle x y z
            triples list
                | length list < 3 = []
                | otherwise       = (setsof3 list) ++ (triples . tail $ list)
            setsof3 (x : xs)
                | length xs < 2 = []
                | otherwise     = (map (\l -> x : l) (setsof2 xs)) ++ (setsof3 (x : tail xs))
            setsof2 (x : xs)
                | length xs < 1 = [[]]
                | length xs == 1= [[x, last xs]]
                | otherwise = [x, head xs] : (setsof2 (x : tail xs))

process :: String -> IO ()
process file = mapM_ (\x -> putStrLn . show $ x) (triangles . toGeodesic $ file)

main = do
    args <- getArgs
    file <- readFile (args!!0)
    process file
    putStrLn ""
