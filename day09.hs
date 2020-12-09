import Data.Set as Set
import Data.List as List

sz = 25

f x s = not . (Set.empty ==) . Set.filter (\e -> (x - e) `Set.member` s || (e - x) `Set.member` s) $ s

part1 (x:xs) = if f b s then part1 xs else b
  where 
    s = Set.fromList . List.take sz $ (x:xs)
    b = head . List.drop sz $ (x:xs)

part2 :: Int -> [Int] -> Int
part2 val (x:xs) = if a == [] then part2 val xs else (List.minimum . head $ a) + (List.maximum . head $ a)
  where
    a = List.filter ((==val) . sum) . List.inits $ (x:xs)

main :: IO ()
main = do
  str <- readFile "input.txt"
  let input = List.map read . lines $ str  :: [Int]
  print $ part1 input
  print $ part2 (part1 input) input
