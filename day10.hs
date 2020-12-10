import Data.List as List

part1 :: [Int] -> Int
part1 input = three * one
  where
    jolts = sort input
    diffs = zipWith (-) (tail jolts) jolts
    three = (1 +) . length . List.filter (3 ==) $ diffs
    one = (1 +) . length . List.filter (1 ==) $ diffs

part2 :: [Int] -> Int
part2 x = product . List.map (\l -> if length l < 4 then 2 ^ (length l - 1) else 7) . List.filter (elem 1) . group $ diffs
    where 
      jolts = sort x
      diffs = zipWith (-) (tail jolts) jolts

main :: IO ()
main = do
  str <- readFile "input.txt"
  let input = List.map read . lines $ str :: [Int]
  print $ part1 input
  print $ part2 (0:input)
-- 0, 1, 4, 5, 6, 7, 10, 11, 12, 15, 16, 19
-- 1  1  3  2  1  1   2   1   1   1   1   1
