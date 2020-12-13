import Data.List as List
import Data.List.Split

part1 :: Int -> [Int] -> Int
part1 n xs = (\(a, b) -> a * b) . head . sort . map (\x -> (\y -> ((y - n), x)) . head . dropWhile (< n) $ [x * i | i <- [1..]]) $ xs

part2 :: Integer -> [(Integer, Integer)] -> Integer
part2 n xs = (+1) . fst . crt . map (\(x,y) -> (-x, y)) $ xs

main :: IO ()
main = do
  str <- readFile "input.txt"
  let [n, input] = lines $ str
  print $ part1 (read n) (map (read) . filter (/= "x") . Data.List.Split.splitOn "," $ input)
  print $ part2 (read n) (map (\(x,y) -> (x, read y)) . filter ((/= "x") . snd) . zip ([1..]) . Data.List.Split.splitOn "," $ input)

-- copied from https://stackoverflow.com/a/35529381/11293511
crt :: (Integral a, Foldable t) => t (a, a) -> (a, a)
crt = foldr go (0, 1)
    where
    go (r1, m1) (r2, m2) = (r `mod` m, m)
        where
        r = r2 + m2 * (r1 - r2) * (m2 `inv` m1)
        m = m2 * m1

    -- Modular Inverse
    a `inv` m = let (_, i, _) = gcd a m in i `mod` m

    -- Extended Euclidean Algorithm
    gcd 0 b = (b, 0, 1)
    gcd a b = (g, t - (b `div` a) * s, s)
        where (g, s, t) = gcd (b `mod` a) a
