import qualified Geo
import System.Environment (getArgs)
import Text.Regex.Posix
import Data.List

matches :: String -> String -> [String]
matches pattern text = (tail . concat) (text =~ pattern :: [[String]])

extractCoords :: String -> [String]
extractCoords line = matches "pll=(.+)\\|(.+)" line

toGeodesic :: String -> [Geo.Geodesic]
toGeodesic s = map (uncurry Geo.Geodesic . tuple . map (\x -> read x :: Double)) coords
    where   coords       = map (matches "(.+),(.+)" . head . extractCoords) $ lines s
            tuple [x, y] = (x, y)

triangles :: [Geo.Geodesic] -> [Geo.Triangle]
triangles list = map toTri (triples list)
    where   toTri [x, y, z] = Geo.Triangle x y z
            triples list
                | length list < 3 = []
                | otherwise       = (setsof3 list) ++ (triples . tail $ list)
            setsof3 (x : xs)
                | length xs < 2   = []
                | otherwise       = (map (\l -> x : l) (setsof2 xs)) ++ (setsof3 (x : tail xs))
            setsof2 (x : xs)
                | length xs == 1  = [[x, last xs]]
                | otherwise = [x, head xs] : (setsof2 (x : tail xs))

process :: String -> IO ()
process file = mapM_ (\x -> putStrLn . show $ x) (triangles . toGeodesic $ file)

main = do
    args <- getArgs
    file <- readFile (args!!0)
    process file
    putStrLn ""
