parse :: Char -> Int
parse '#' = 1
parse _ = 0

part1 :: [String] -> (Int, Int) -> Int
part1 = countTrees 0 0 
  where
    countTrees :: Int -> Int -> [String] -> (Int, Int) -> Int
    countTrees _ result [] _ = result
    countTrees x result (input:inputs) slope =
      countTrees
        ((x + fst slope) `mod` (length input))
        (parse (input !! x) + result)
        (drop ((snd slope) - 1) inputs)
        slope
        
part2 :: [String] -> [(Int, Int)] -> Int
part2 input = product . map (part1 input)

main :: IO ()
main = do
  str <- readFile "input.txt"
  let input = lines $ str
  print $ part1 input (3, 1)
  print $ part2 input [(1, 1), (3, 1), (5, 1), (7, 1), (1, 2)]
