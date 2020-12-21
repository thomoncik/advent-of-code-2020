import Data.List
import Data.List as List
import Data.List.Split as Split
import Data.Map as Map
import Data.Set as Set
import Debug.Trace

-- part1 :: [[[String]]] -> Map String (Set String) -> Map String (Set String)
part1 [] m = Set.unions . List.map snd . Map.toList $ m
part1 ([ingredients, alergens]:xs) m = part1 xs m'
  where m' = List.foldl (\mm (a,i) -> Map.insertWith Set.intersection a i mm) m [(a,Set.fromList ingredients) | a <- alergens]

part2 [] m = Map.toList . Map.map (Set.toList) . Map.filter (not . Set.null) $ m
-- part2 [] m = concat . intersperse "," . List.nub . List.concat . List.map snd . Map.toList . Map.map (Set.toList) . Map.filter (not . Set.null) $ m
part2 ([ingredients, alergens]:xs) m = part2 xs m'
  where m' = List.foldl (\mm (a,i) -> Map.insertWith Set.intersection a i mm) m [(a,Set.fromList ingredients) | a <- alergens]

main :: IO ()
main = do
  str <- readFile "input.txt"
  let input =
        List.map
          (List.map
             (List.map (List.filter (/= ',')) .
              List.filter (/= "contains") . words) .
           List.filter (/= []) . Split.splitOneOf "()") .
        lines $
        str
  -- print $ input
  
  let allEl = List.foldl (\s [x,_] -> Set.union s (Set.fromList x)) Set.empty input
  let aler = part1 input Map.empty
  
  let okFood = Set.difference allEl aler
  print $ Set.foldl (\res x -> res + (length . List.filter (==x) . words $ str)) 0 okFood
  
  ----

  print $ part2 input Map.empty

  -- Helper map then just arraged it here

  -- [("dairy",["ppdplc"]),
  -- ("eggs",["gkcplx"]),
  -- ("fish",["ktlh"]),
  -- ("nuts",[msfmt"]),
  -- ("sesame",["dqsbql"]),
  -- ("shellfish",[mvqkdj"]),
  -- ("soy",["ggsz"]),
  -- ("wheat",["hbhsx"])]

  -- ppdplc,gkcplx,ktlh,msfmt,dqsbql,mvqkdj,ggsz,hbhsx