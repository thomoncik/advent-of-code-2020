part1 :: [Integer] -> Integer
part1 input = head [i * j | i <- input, j <- input, i + j == 2020]

part2 :: [Integer] -> Integer
part2 input = head [i * j * k | i <- input, j <- input, k <- input, k + i + j == 2020]

main :: IO ()
main = do
  str <- readFile "input.txt"
  let input = map read . lines $ str
  print $ part1 input
  print $ part2 input
