import Data.List.Split
import Data.List (findIndex, isPrefixOf, tails, sort, filter, foldr)
import Data.Maybe
import qualified Data.Map
import qualified Data.Set as Set
import Debug.Trace

dropLast :: [a] -> [a]
dropLast [] = []
dropLast [x] = []
dropLast (x:xs) = x : (dropLast xs)

parseBag [c, i, j] = (read c :: Int, i ++ " " ++ j)
parseBag _ = (0, "")

parseLine line = map (\e -> (a, e)) x
  where 
    [a, b] = Data.List.Split.splitOn " bags contain" line
    x = map (parseBag . dropLast . words) . Data.List.Split.splitOneOf "," $ b

-- f :: Set.Set String -> String -> [(String, (Int, String))] -> Set.Set String
-- f v str x
--   | filter (\(a, (b, c)) -> c == str && a `Set.notMember` v) x == [] = v'
--   | otherwise = foldr Set.union v . map(\(a, (b, c)) -> f v' a x) . filter (\(a, (b, c)) -> c == str && a `Set.notMember` v) $ x
--   where 
--     v' = Set.union v . Set.fromList . map (\(a, (b, c)) -> a) . filter (\(a, (b, c)) -> c == str && a `Set.notMember` v) $ x


f :: String -> [(String, (Int, String))] -> Int
f str x = sum . map (\(a, (b, c)) -> b * f c x + b) . filter (\(a, (b, c)) -> a == str) $ x
    

main :: IO ()
main = do
  str <- readFile "input.txt"
  let input = lines str
  print $ f "shiny gold" . concat . map parseLine $ input