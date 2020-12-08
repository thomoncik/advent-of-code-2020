module Main (main) where

import Data.Char as Char
import Data.List as List
import Data.Map as Map
import Data.Set as Set

import Debug.Trace

data Instruction
  = Acc Int
  | Jmp Int
  | Nop Int
  deriving (Show, Read, Eq)

type Program = Map.Map Int Instruction

type State = (Int, Int) -- cursor, accumulator

readInstruction :: String -> Instruction
readInstruction line = readInstruction' (words line)
  where
    readInstruction' [(o:operator), ('+':number)]     = read $ (Char.toUpper o) : operator ++ " " ++ number
    readInstruction' [(o:operator), (negativeNumber)] = read $ (Char.toUpper o) : operator ++ negativeNumber

programStep :: State -> Instruction -> State
programStep (cursor, accumulator) (Acc arg) = (cursor + 1, accumulator + arg)
programStep (cursor, accumulator) (Nop arg) = (cursor + 1, accumulator)
programStep (cursor, accumulator) (Jmp arg) = (cursor + arg, accumulator)

runUntilVisitedAgain :: Set.Set Int -> State -> Program -> Int
runUntilVisitedAgain visited state@(cursor, accumulator) program
  | Set.member cursor' visited = accumulator
  | otherwise = runUntilVisitedAgain (Set.insert cursor' visited) state' program
  where
    state'@(cursor', accumulator') = programStep state (program Map.! cursor)

part1 :: [String] -> Int
part1 =
  runUntilVisitedAgain Set.empty (1, 0) . Map.fromList . zip [1 ..] . List.map readInstruction

----------------------------------------------------
main :: IO ()
main = do
  str <- readFile "input.txt"
  let input = lines str
  print $ part1 input
