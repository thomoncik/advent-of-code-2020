import Data.List.Split
import Data.Set (size, fromList, intersection)

part1 = sum . map (size . fromList . concat)
part2 = sum . map (size . foldr (intersection . fromList) (fromList ['a'..'z']))

main :: IO ()
main = do
  str <- readFile "input.txt"
  let input = Data.List.Split.splitOn [""] . Data.List.Split.splitOn "\n" $ str
  print $ part1 input
  print $ part2 input
