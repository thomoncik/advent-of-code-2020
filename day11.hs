module Main
  ( main
  ) where

import Data.List as List
import Data.Map as Map
import Data.Set as Set
import Data.Maybe

import Debug.Trace

type Coordinates = (Int, Int)

zipWith2dIndex :: [[a]] -> [(Coordinates, a)]
zipWith2dIndex xss =
  [((i, j), x) | (j, xs) <- zip [0 ..] xss, (i, x) <- zip [0 ..] xs]

parse :: [String] -> Map Coordinates Char
parse inputLines = Map.fromList (zipWith2dIndex inputLines)

-- step :: Map Coordinates Char -> Map Coordinates Char
-- step m = Map.fromList [((x, y), c (x, y)) | y <- [0 .. maxy], x <- [0 .. maxx]]
--   where 
--     c (x, y) = c' (m Map.! (x, y)) (List.length . List.filter (Just '#'==) . adj $ (x, y))
--     c' 'L' 0 = '#'
--     c' '#' a = if a > 4 then 'L' else '#'
--     c' s _ = s
--     adj (x, y) = List.map (m Map.!?) [(x + a, y + b) | a <- [-1..1], b <- [-1..1]]
--     (maxx, maxy) = fst . Map.findMax $ m

-- part1 visited board
--   -- | Set.member board visited = trace (drawMap board) List.length . Map.filter ('#'==) $ board
--   -- | otherwise = trace (drawMap board) part1 (Set.insert board visited) (step board)
--   | Set.member board visited = List.length . Map.filter ('#'==) $ board
--   | otherwise = part1 (Set.insert board visited) (step board)

firstInDir m (x, y) dirx diry = head . dropWhile ((== Just '.')) $ [m Map.!? (x + a * dirx, y + a * diry) | a <- [1..]]

step :: Map Coordinates Char -> Map Coordinates Char
step m = Map.fromList [((x, y), c (x, y)) | y <- [0 .. maxy], x <- [0 .. maxx]]
  where 
    c (x, y) = c' (m Map.! (x, y)) (adj $ (x, y))
    c' 'L' 0 = '#'
    c' '#' a = if a > 4 then 'L' else '#'
    c' s _ = s
    adj (x, y) = sum [if firstInDir m (x, y) a b == Just '#' then 1 else 0 | a <- [-1..1], b <- [-1..1], (a, b) /= (0, 0)]
    (maxx, maxy) = fst . Map.findMax $ m

part2 prev board
  | board == prev = trace (drawMap board) List.length . Map.filter ('#'==) $ board
  | otherwise = trace (drawMap board) part2 board (step board)

drawMap :: Map Coordinates Char -> [Char]
drawMap m = [c (x, y) | y <- [0 .. maxy], x <- [0 .. maxx + 1]]
  where 
    c (x, y) = if m Map.!? (x,y) == Nothing then '\n' else m Map.! (x,y)
    (maxx, maxy) = fst . Map.findMax $ m

main :: IO ()
main = do
  str <- readFile "input.txt"
  let input = lines str
  -- print $ part1 Set.empty . parse $ input
  print $ part2 Map.empty . parse $ input
