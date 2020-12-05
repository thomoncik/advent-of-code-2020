import Data.List (sort)
import qualified Data.Set

digitToInt :: Char -> Int
digitToInt 'B' = 1
digitToInt 'R' = 1
digitToInt _ = 0

seatId :: String -> Int
seatId = foldl (\acc x -> acc * 2 + digitToInt x) 0

part1 :: [String] -> Int
part1 = maximum . map seatId

pairs [] = []
pairs (x:xs) = zip (x:xs) xs

part2 :: [String] -> Int
part2 input = (+1) . fst . head . filter (\(a, b) -> a /= b - 1) . pairs . sort . map seatId $ input

main :: IO ()
main = do
  str <- readFile "input.txt"
  let input = lines str
  print $ part1 input
  print $ part2 input
