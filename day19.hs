import Data.List.Split
import Data.List
import Data.List as List
import Data.Map as Map


messageValid :: Map String String -> String -> [String] -> Bool
messageValid rules "" r = r == []
messageValid rules _ [] = False
messageValid rules (s:ss) (r:rs)
  | not $ Map.member r rules = False
  | Map.member r rules && s == head (rules Map.! r) = messageValid rules ss rs
  | otherwise = any (\x -> messageValid rules (s:ss) ((Data.List.Split.splitOn " " x) ++ rs)) . Data.List.Split.splitOn " | " $ (rules Map.! r)

main :: IO ()
main = do
  str <- readFile "input.txt"
  let input = str
  let [rules, tests] = Data.List.Split.splitOn "\n\n" input

  let rulesMap = Map.fromList $ List.map ((\[a, b] -> (a, List.filter (/= '"') b)) . Data.List.Split.splitOn ": ") . lines $ rules
  let testsList = lines tests

  print $ List.foldl (\i v -> if v then i + 1 else i) 0 . List.map (\t -> messageValid rulesMap t ["0"]) $ testsList
  let rulesMap' = Map.union (Map.fromList [("8", "42 | 42 8"), ("11", "42 31 | 42 11 31")]) rulesMap
  print $ List.foldl (\i v -> if v then i + 1 else i) 0 . List.map (\t -> messageValid rulesMap' t ["0"]) $ testsList