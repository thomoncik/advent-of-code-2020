import Data.List as List
import Data.List.Split as Split
import Data.Set as Set
import Data.Sequence as Seq

part1 [a, []] = List.sum . List.map (\(x,y) -> x*y) $ List.zip (List.reverse a) [1..]
part1 [[], b] = List.sum . List.map (\(x,y) -> x*y) $ List.zip (List.reverse b) [1..]
part1 [p1:x, p2:y] | p1 > p2   = part1 [x List.++ [p1, p2], y]
                   | otherwise = part1 [x, y List.++ [p2, p1]]

--

part2 :: Set (Seq Int, Seq Int) -> (Seq Int, Seq Int) -> (Int, Int)
part2 _ (xs, Seq.Empty) = (0, Seq.foldlWithIndex (\a y x -> a + x*((Seq.length xs) - y)) 0 xs)
part2 _ (Seq.Empty, ys) = (1, Seq.foldlWithIndex (\a y x -> a + x*((Seq.length ys) - y)) 0 ys)
part2 s deck@((p1 :<| x), (p2 :<| y))
  | Set.member deck s = (0, 0)
  | p1 <= Seq.length x && p2 <= Seq.length y && fst (part2 Set.empty (Seq.take p1 x, Seq.take p2 y)) == 0 = part2 (Set.insert deck s) (x |> p1 |> p2, y)
  | p1 <= Seq.length x && p2 <= Seq.length y = part2 (Set.insert deck s) (x, y |> p2 |> p1)
  | p1 > p2   = part2 (Set.insert deck s) (x |> p1 |> p2, y)
  | otherwise = part2 (Set.insert deck s) (x, y |> p2 |> p1)

main :: IO ()
main = do
  str <- readFile "input.txt"
  let input = List.map (List.map read) . List.map (List.filter ((/='P') . List.head) . lines) . Split.splitOn "\n\n" $ str :: [[Int]]
  print $ part1 input
  let
    [a, b] = input
    input2 = (Seq.fromList a, Seq.fromList b)
  print $ snd $ part2 Set.empty input2