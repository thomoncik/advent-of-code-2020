import Data.List
import Data.List as List
import Data.List.Split as Split
import Data.Map as Map
import Data.Set as Set
import Debug.Trace
import Data.Function

decodePosition :: (Int, Int) -> String -> (Int, Int)
decodePosition pos [] = pos
decodePosition (x, y) ('e':ss)     = decodePosition (x + 1, y) ss
decodePosition (x, y) ('n':'e':ss) = decodePosition (x, y + 1) ss
decodePosition (x, y) ('n':'w':ss) = decodePosition (x - 1, y + 1) ss
decodePosition (x, y) ('w':ss)     = decodePosition (x - 1, y) ss
decodePosition (x, y) ('s':'w':ss) = decodePosition (x, y - 1) ss
decodePosition (x, y) ('s':'e':ss) = decodePosition (x + 1, y - 1) ss

filterOddFrequency :: (Ord a) => [a] -> Set a
filterOddFrequency xs = Map.keysSet . Map.filter odd  . fromListWith (+) $ [(x, 1) | x <- xs]

part1 :: [String] -> Int
part1 = Set.size . filterOddFrequency. List.map (decodePosition (0,0))

applyRules :: Set (Int, Int) -> Set (Int, Int)
applyRules s = Set.fromList [(x, y) | x <- [minx .. maxx], y <- [miny .. maxy], next (x, y)]
  where 
    minx = (+(-1)) . minimum . Set.map fst $ s
    miny = (+(-1)) . minimum . Set.map snd $ s
    maxx = (+1) . maximum . Set.map fst $ s
    maxy = (+1) . maximum . Set.map snd $ s
    neighbours (x, y) = sum [1 | (a,b) <- [(1,0), (0,1), (-1,1), (-1,0), (0,-1), (1,-1)], Set.member (x + a, y + b) s]
    next (x, y)
      | Set.member (x, y) s && (neighbours (x, y) == 0 || neighbours (x, y) > 2) = False
      | Set.notMember (x, y) s && neighbours (x, y) == 2 = True
      | otherwise = Set.member (x, y) s

part2 :: [String] -> Int
part2 = Set.size . (!! 100) . iterate applyRules . filterOddFrequency . List.map (decodePosition (0,0))

main :: IO ()
main = do
  str <- readFile "input.txt"
  let input = lines str
  print $ part1 input
  print $ part2 input