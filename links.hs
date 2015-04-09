import System.Environment (getArgs)
import Text.Regex.Posix
import Text.Printf
import Data.List

import qualified Geo

matches :: String -> String -> [String]
matches pattern text = (tail . concat) (text =~ pattern :: [[String]])

extractCoords :: String -> [String]
extractCoords line = matches "pll=(.+)\\|(.+)" line

name :: String -> Geo.Geodesic -> String
name file (Geo.Geodesic lat long) = last $ matches pattern file
    where pattern = printf "pll=%f,%f\\|(.+)" lat long

nameTri :: String -> Geo.Triangle -> String
nameTri file (Geo.Triangle a b c) = printf "[%s, %s, %s]" (n a) (n b) (n c)
    where n = name file

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

count :: [Geo.Geodesic] -> Geo.Triangle -> (Int, Geo.Triangle)
count points t = (length (filter (\p -> Geo.containsPoint t p) points), t)

bestTriangle :: [Geo.Geodesic] -> Geo.Triangle
bestTriangle points = snd . maximum $ map (count points) $ triangles points

process :: String -> IO ()
process file = mapM_ (\x -> putStrLn . show $ x) (triangles . toGeodesic $ file)

main = do
    args <- getArgs
    file <- readFile (args!!0)
    process file
    putStrLn ""
    putStrLn (nameTri file . bestTriangle . toGeodesic $ file)
    putStrLn ""
    -- mapM_ (\(x,g) -> do
    --   putStr $ show x ++ " "
    --   putStrLn $ nameTri file g) (filter (\(x, g) -> x > 0) $ map (count (toGeodesic file)) (triangles . toGeodesic $ file))

