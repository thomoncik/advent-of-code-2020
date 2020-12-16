import Data.List.Split
import Data.List
import Data.Char
import Data.Function (on)

import Debug.Trace

part1 :: [[Int]] -> [Int] -> [Int] -> Int
part1 rules _ nearby = sum . filter ((\x -> isnotok x rules)) $ nearby

isnotok :: Int -> [[Int]] -> Bool
isnotok x [] = True
isnotok x ([a,b,c,d]:rs)
  | (a <= x && x <= b) || (c <= x && x <= d) = False
  | otherwise = isnotok x rs

valid :: [[Int]] -> [Int] -> Bool
valid rules ticket = [] == filter (\x -> isnotok x rules) ticket

f rules valid_nearby = map (map fst . filter snd . possible_cols) . transpose $ valid_nearby
  where possible_cols l = zip [1..] $ map (all (\a -> a)) $ transpose $ map (\x -> (map (\[a,b,c,d] -> (a <= x && x <= b) || (c <= x && x <= d)) rules)) l

part2 rules your valid_nearby = product . map ((!!) your . (+ (-1)) . fst) . filter ((<= 6) . snd) $ foo
  where 
    foo = tail $ map (\(x,y,_) -> (x,y)) $ scanl (\(x,e,l) [(y, l')] -> (y, head (l' \\ l), l')) (0,0,[]) inc
    inc = groupBy ((==) `on` length . snd) $ sortBy (compare `on` length . snd) $ zip [1..] (f rules valid_nearby)

main :: IO ()
main = do
  str <- readFile "input.txt"
  let input = str
  let [rules_str, ("your":("ticket":your_str:[])), ("nearby":("tickets":nearby_str))] = map (filter ("" /=) . Data.List.Split.splitOneOf " :-\n") . Data.List.Split.splitOn "\n\n" $ input
  
  let rules = map (map read) . Data.List.Split.chunksOf 4 . filter (isDigit . head) $ rules_str :: [[Int]]
  let your = (map read . Data.List.Split.splitOn "," $ your_str :: [Int])
  let nearby = (map (map read . Data.List.Split.splitOn ",") $ nearby_str :: [[Int]])
  
  let valid_nearby = filter (valid rules) nearby

  print $ part1 rules your (concat nearby)
  print $ part2 rules your valid_nearby
