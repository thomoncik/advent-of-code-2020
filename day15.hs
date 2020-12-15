import Data.List.Split
import qualified Data.IntMap as IntMap

import Debug.Trace

part1 :: Int -> IntMap.IntMap Int -> [Int] -> Int
part1 30000000 m (x:[]) = x
part1 i m (x:[]) = part1 (i + 1) (IntMap.insert x i m) (x':[])
  where x' = if (x `IntMap.member` m) then i - (m IntMap.! x) else 0
part1 i m (x:xs) = part1 (i + 1) (IntMap.insert x i m) xs

main :: IO ()
main = do
  str <- readFile "input.txt"
  let input = str
  -- print $ part1 1 2020 Map.empty (map read . Data.List.Split.splitOn "," $ input)
  print $ part1 1 IntMap.empty (map read . Data.List.Split.splitOn "," $ input)
