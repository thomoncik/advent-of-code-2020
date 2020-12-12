import Data.List as List
import Debug.Trace

parseLine :: String -> (Char, Float)
parseLine (x:xs) = (x, read xs)

parse :: [String] -> [(Char, Float)]
parse input = List.map parseLine input

part1 :: (Float, Float)  -> [(Char, Float)] -> (Float, Float) -> Int
part1 (dx, dy) [] (x, y) =  trace (show (x,y) ++ " " ++ show (dx, dy)) abs(round x) + abs(round y)
part1 (dx, dy) (('S', d):ds) (x, y) = trace (show (x,y) ++ " " ++ show (dx, dy)) part1 (dx, dy) ds (x, y - d)
part1 (dx, dy) (('E', d):ds) (x, y) = trace (show (x,y) ++ " " ++ show (dx, dy)) part1 (dx, dy) ds (x + d, y)
part1 (dx, dy) (('N', d):ds) (x, y) = trace (show (x,y) ++ " " ++ show (dx, dy)) part1 (dx, dy) ds (x, y + d)
part1 (dx, dy) (('W', d):ds) (x, y) = trace (show (x,y) ++ " " ++ show (dx, dy)) part1 (dx, dy) ds (x - d, y)
part1 (dx, dy) (('L', d):ds) (x, y) = trace (show (x,y) ++ " " ++ show (dx, dy)) part1 (dx * a - (dy * b), dx * b + dy * a) ds (x, y)
  where rad = d * pi / 180.0
        (a, b) = (cos(rad), sin(rad))
part1 (dx, dy) (('R', d):ds) (x, y) = trace (show (x,y) ++ " " ++ show (dx, dy)) part1 (dx * a + dy * b, dx * (-b) + dy * a) ds (x, y)
  where rad = d * pi / 180
        (a, b) = (cos(rad), sin(rad))
part1 (dx, dy) (('F', d):ds) (x, y) = trace (show (x,y) ++ " " ++ show (dx, dy)) part1 (dx, dy) ds (x + (d * dx), y + (d * dy))


part2 :: (Float, Float)  -> [(Char, Float)] -> (Float, Float) -> Int
part2 (dx, dy) [] (x, y) =  trace (show (x,y) ++ " " ++ show (dx, dy)) abs(round x) + abs(round y)
part2 (dx, dy) (('S', d):ds) (x, y) = trace (show (x,y) ++ " " ++ show (dx, dy)) part2 (dx, dy - d) ds (x, y)
part2 (dx, dy) (('E', d):ds) (x, y) = trace (show (x,y) ++ " " ++ show (dx, dy)) part2 (dx + d, dy) ds (x, y)
part2 (dx, dy) (('N', d):ds) (x, y) = trace (show (x,y) ++ " " ++ show (dx, dy)) part2 (dx, dy + d) ds (x, y)
part2 (dx, dy) (('W', d):ds) (x, y) = trace (show (x,y) ++ " " ++ show (dx, dy)) part2 (dx - d, dy) ds (x, y)
part2 (dx, dy) (('L', d):ds) (x, y) = trace (show (x,y) ++ " " ++ show (dx, dy)) part2 (dx * a - (dy * b), dx * b + dy * a) ds (x, y)
  where rad = d * pi / 180.0
        (a, b) = (cos(rad), sin(rad))
part2 (dx, dy) (('R', d):ds) (x, y) = trace (show (x,y) ++ " " ++ show (dx, dy)) part2 (dx * a + dy * b, dx * (-b) + dy * a) ds (x, y)
  where rad = d * pi / 180
        (a, b) = (cos(rad), sin(rad))
part2 (dx, dy) (('F', d):ds) (x, y) = trace (show (x,y) ++ " " ++ show (dx, dy)) part2 (dx, dy) ds (x + (d * dx), y + (d * dy))

main :: IO ()
main = do
  str <- readFile "input.txt"
  let input = lines $ str
  -- print $ part1 (1, 0) (parse input) (0, 0)
  print $ part2 (10, 1) (parse input) (0, 0)
