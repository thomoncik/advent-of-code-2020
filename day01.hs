import Data.List

part1 :: [Integer] -> Integer
part1 input = head [x * y | x:xs <- tails input, y <- xs, x + y == 2020]

part2 :: [Integer] -> Integer
part2 input = head [x * y * z | x:xs <- tails input, y:ys <- tails xs, z <- ys, x + y + z == 2020]

main :: IO ()
main = do
  str <- readFile "input.txt"
  let input = map read . lines $ str
  print $ part1 input
  print $ part2 input
