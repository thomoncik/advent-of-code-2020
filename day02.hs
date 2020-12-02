import Data.List.Split

data Password = Password Int Int Char String

lineToPassword :: String -> Password
lineToPassword str = 
  Password (read low) (read high) (head character) password
    where
      [limits, character, password] = words str
      [low, high] = splitOn "-" limits

valid :: Password -> Bool
valid (Password low high c pass) = (len >= low) && (len <= high)
  where len = length . filter (== c) $ pass

part1 :: [Password] -> Int
part1 = length . filter valid

valid2 :: Password -> Bool
valid2 (Password low high c pass) = a /= b
  where
    a = pass !! (low - 1) == c
    b = pass !! (high - 1) == c

part2 :: [Password] -> Int
part2 = length . filter valid2

main :: IO ()
main = do
  str <- readFile "input.txt"
  let input = map lineToPassword . lines $ str
  print $ part1 input
  print $ part2 input
