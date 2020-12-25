import Data.List as List
import Data.Map as Map
import Data.Set as Set

findSolution :: Int -> Int -> Int -> Int -> Int
findSolution doorPub cardPub target key
  | target == doorPub = key
  | otherwise = findSolution doorPub cardPub ((target * 7) `mod` 20201227) ((key * cardPub) `mod` 20201227)

part1 :: Int -> Int -> Int
-- part1 = (+)
part1 doorPub cardPub = findSolution doorPub cardPub 1 1

main :: IO ()
main = do
  str <- readFile "input.txt"
  let [cardPub, doorPub] = List.map read . lines $ str
  print $ part1 doorPub cardPub
  -- print $ part2 input