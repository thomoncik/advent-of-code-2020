import Data.List.Split
import qualified Data.Map as Map
import Data.Char (isDigit)

readCsvIntegers :: String -> [Integer]
readCsvIntegers = map read . Data.List.Split.splitOn " "

pairs :: [a] -> [(a, a)]
pairs [] = []
pairs (x:y:xs) = (x, y):(pairs xs)
pairs _ = []

parseLine :: String -> Map.Map String String
parseLine = Map.fromList . pairs . Data.List.Split.splitOneOf ": "

valid :: Map.Map String String -> Bool
valid x = all (== True) . map (`Map.member` x) $ ["byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid"]

part1 =  length . filter (== True) . map (valid . parseLine)


betterValid :: Map.Map String String -> Bool
betterValid x = all (== True) [byr, iyr, eyr, hgt, hcl, ecl, pid]
  where
    byr = (\v -> v >= 1920 && v <= 2002) . read $ x Map.! "byr"
    iyr = (\v -> v >= 2010 && v <= 2020) . read $ x Map.! "iyr"
    eyr = (\v -> v >= 2020 && v <= 2030) . read $ x Map.! "eyr"
    hgt = validHeight $ x Map.! "hgt"
    hcl = (\v -> head v == '#' && length v == 7) $ x Map.! "hcl"
    ecl = (\v -> (/= 0) . length . filter (== v) $ ["amb", "blu", "brn", "gry", "grn", "hzl", "oth"]) $ x Map.! "ecl" 
    pid = (\v -> (== 9) . length . filter isDigit $ v) $ x Map.! "pid"

validHeight h
  | fmt && y == "cm" = v >= 150 && v <= 193
  | fmt && y == "in" = v >= 59 && v <= 76
  | otherwise = False
  where
    fmt = (length x == 3) && if all isDigit . head $ x then True else False
    x = Data.List.Split.splitWhen (not . isDigit) $ h
    y = dropWhile isDigit h
    v = read (takeWhile isDigit h) :: Integer

part2 =  length . filter (== True) . map betterValid . filter valid . map parseLine

main :: IO ()
main = do
  str <- readFile "input.txt"
  let input = map (map (\x -> if x == '\n' then ' ' else x)) . Data.List.Split.splitOn "\n\n" $ str
  print $ part1 input
  print $ part2 input