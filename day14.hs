import Data.List as List
import Data.List.Split
import Data.Map as Map
import Data.Set as Set

import Debug.Trace

toBinary :: Integer -> [Integer]
toBinary n = reverse [n `div` 2^x `rem` 2  | x <- [0..35]]

fromBinary :: [Integer] -> Integer
fromBinary n = sum [2^x | x <- reverse [0..35], n !! (35 - x) == 1]

part1 :: Map Integer Integer -> Map Integer Integer -> [[String]] -> Integer
part1 mem mask [] = Map.foldl' (+) 0 mem
part1 mem _ ((("mask"):mask:[]):ls) = trace (mask) part1 mem (Map.map (\c -> if c == '1' then 1 else 0) . Map.filter (/= 'X') . Map.fromList . zip ([1..]) $ mask) ls
part1 mem mask ((("mem"):addr:val:[]):ls) = trace (show mask ++ " # " ++ addr ++ " <- " ++ (show val')) part1 mem' mask ls
  where
    addr' = read addr :: Integer
    val' = trace (show $ zip [1..] $ toBinary $ read val) fromBinary . List.map (\(i,x) -> if Map.member i mask then mask Map.! i else x) . zip [1..] . toBinary . read $ val
    mem' = Map.insert addr' val' mem

part2 :: Map Integer Integer -> Map Integer Integer -> [[String]] -> Integer
part2 mem mask [] = Map.foldl' (+) 0 mem
part2 mem mask ((("mask"):m:[]):ls) = part2 mem (Map.map (\c -> if c == '1' then 1 else if c == 'X' then -1 else 0) . Map.fromList . zip [1..] $ m) ls
part2 mem mask ((("mem"):addr:val:[]):ls) = part2 mem' mask ls 
  where
    hs = Map.filter (-1 ==) $ mask
    ssqs = List.map (\os -> Map.unions [(Map.map (\v -> 1) . Map.fromList $ os), (Map.map (\v -> -1) hs), mask]) . subsequences . Map.toList $ hs
    addrs' = List.map (\mask -> fromBinary . List.map (\(i,x) -> if mask Map.!? i == Just 1 then 1 else if mask Map.!? i == Just (-1) then 0 else x) . zip [1..] . toBinary . read $ addr) ssqs
    val' = read val :: Integer
    mem' = List.foldr (\a m -> Map.insert a val' m) mem addrs'

main :: IO ()
main = do
  str <- readFile "input.txt"
  let input = lines $ str
  -- print $ part1 Map.empty Map.empty . List.map (List.filter ("" /=) . Data.List.Split.splitOneOf "[]= ") $ input
  print $ part2 Map.empty Map.empty . List.map (List.filter ("" /=) . Data.List.Split.splitOneOf "[]= ") $ input
