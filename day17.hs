module Main
  ( main
  ) where

import Data.List as List
import Data.Map as Map
import Data.Set as Set
import Data.Maybe

import Debug.Trace

-- type Coordinates = (Int, Int, Int)

-- zipWith3dIndex :: [[a]] -> [(Coordinates, a)]
-- zipWith3dIndex xss =
--   [((i, j, 0), x) | (j, xs) <- zip [0 ..] xss, (i, x) <- zip [0 ..] xs]

-- parse :: [String] -> Map Coordinates Char
-- parse inputLines = Map.fromList (zipWith3dIndex inputLines)

-- step :: Map Coordinates Char -> Map Coordinates Char
-- step m = Map.fromList [((x, y, z), c (x, y, z)) | y <- [miny - 1 .. maxy + 1], x <- [minx - 1 .. maxx + 1], z <- [minz - 1 .. maxz + 1]]
--   where 
--     c (x, y, z) = c' (Map.findWithDefault '.' (x, y, z) m) (List.length . List.filter (Just '#'==) . adj $ (x, y, z))
--     c' '.' 3 = '#'
--     c' '.' _ = '.'
--     c' '#' 2 = '#'
--     c' '#' 3 = '#'
--     c' '#' _ = '.'
--     adj (x, y, z) = List.map (m Map.!?) [(x + a, y + b, z + c) | a <- [-1..1], b <- [-1..1], c <- [-1..1], (a,b,c) /= (0,0,0)]
--     (maxx, maxy, maxz) = fst . Map.findMax $ m
--     (minx, miny, minz) = fst . Map.findMin $ m

-- part1 0 _ board = trace (drawMap board) List.length . Map.filter ('#'==) $ board
-- part1 n prev board = trace (drawMap board) part1 (n - 1) board (step board)

-- drawMap :: Map Coordinates Char -> [Char]
-- drawMap m = concatMap (++ "\n") (List.map (drawMapLevel m) [z | z <- [minz .. maxz]])
--   where
--     (_, _, maxz) = fst . Map.findMax $ m
--     (_, _, minz) = fst . Map.findMin $ m

-- drawMapLevel :: Map Coordinates Char -> Int -> [Char]
-- drawMapLevel m z = [c (x, y, z) | y <- [0 .. maxy], x <- [0 .. maxx + 1]]
--   where 
--     c (x, y,z) = if m Map.!? (x,y,z) == Nothing then '\n' else m Map.! (x,y,z)
--     (maxx, maxy, _) = fst . Map.findMax $ m


type Coordinates = (Int, Int, Int, Int)

zipWith4dIndex :: [[a]] -> [(Coordinates, a)]
zipWith4dIndex xss =
  [((i, j, 0, 0), x) | (j, xs) <- zip [0 ..] xss, (i, x) <- zip [0 ..] xs]

parse :: [String] -> Map Coordinates Char
parse inputLines = Map.fromList (zipWith4dIndex inputLines)

step :: Map Coordinates Char -> Map Coordinates Char
step m = Map.fromList [((x, y, z, w), c (x, y, z, w)) | y <- [miny - 1 .. maxy + 1], x <- [minx - 1 .. maxx + 1], z <- [minz - 1 .. maxz + 1], w <- [minw - 1 .. maxw + 1]]
  where 
    c (x, y, z, w) = c' (Map.findWithDefault '.' (x, y, z, w) m) (List.length . List.filter (Just '#'==) . adj $ (x, y, z, w))
    c' '.' 3 = '#'
    c' '.' _ = '.'
    c' '#' 2 = '#'
    c' '#' 3 = '#'
    c' '#' _ = '.'
    adj (x, y, z, w) = List.map (m Map.!?) [(x + a, y + b, z + c, w + d) | a <- [-1..1], b <- [-1..1], c <- [-1..1], d <- [-1..1], (a,b,c,d) /= (0,0,0,0)]
    (maxx, maxy, maxz, maxw) = fst . Map.findMax $ m
    (minx, miny, minz, minw) = fst . Map.findMin $ m

part2 0 _ board = List.length . Map.filter ('#'==) $ board
part2 n prev board = part2 (n - 1) board (step board)

main :: IO ()
main = do
  str <- readFile "input.txt"
  let input = lines str
  -- print $ part1 6 Map.empty . parse $ input
  print $ part2 6 Map.empty . parse $ input
