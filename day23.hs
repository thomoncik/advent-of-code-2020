import Data.List as List
import Data.List.Split as Split
import Data.IntMap as IntMap
import Data.Sequence as Seq
import Data.Char as Char
import Debug.Trace
import Control.Monad.ST
import Data.Vector.Unboxed.Mutable as V
import Control.Monad.ST
import Control.Monad

part1 :: Int -> [Int] -> Int
part1 0 xs = List.foldl1 (\x y -> 10*x+y) . List.take 8 . List.drop 1 . List.dropWhile (/=1) $ xs List.++ xs
part1 i (current:x:y:z:xs) = part1 (i - 1) (p1 List.++ [destination, x, y, z] List.++ p2 List.++ [current])
  where 
    destination = List.head [a | a <- List.reverse [1 .. current - 1] List.++ List.reverse [current - 1 .. 9], a /= x && a /= y && a /= z]
    Just destIndex = List.findIndex (==destination) xs
    (p1, (_:p2)) = List.splitAt destIndex xs

calcDestination :: Int -> Int -> (Int, Int, Int) -> Int
calcDestination n current (x, y, z)
  | dest == x || dest == y || dest == z = calcDestination n dest (x, y, z)
  | otherwise = dest
  where dest = (current - 2) `mod` n + 1

part2 :: Int -> Int ->  Int -> V.MVector s Int -> ST s Int
part2 n 0 current cups = do
  x <- V.read cups 1
  y <- V.read cups x
  return (x * y)
part2 n i current cups = do
    x <- V.read cups current
    y <- V.read cups x
    z <- V.read cups y
    let destination = calcDestination n current (x, y, z)
    gap <-  V.read cups z
    join <- V.read cups destination

    _ <- V.write cups current gap
    _ <- V.write cups destination x
    _ <- V.write cups z join
    current' <- V.read cups current
    part2 n (i - 1) current' cups
  
main :: IO ()
main = do
  str <- readFile "input.txt"
  let input = List.map Char.digitToInt str
  print $ part1 100 input

  let n = 1000000
  let x = maximum input
  cups <- do
    cups <- V.new (n + 1)
    zipWithM_ (V.write cups) ((0:input) ++ [x + 1 .. n]) ((0:List.tail input) ++ [x + 1 .. n] ++ [head input])
    return cups

  let current = List.head input
  foo <- stToIO $ part2 n 10000000 current cups
  print foo