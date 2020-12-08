module Main
  ( main
  ) where

import Data.Char as Char
import Data.List as List
import Data.Map as Map
import Data.Maybe
import Data.Set as Set

import Debug.Trace

data Instruction
  = Acc Int
  | Jmp Int
  | Nop Int
  deriving (Show, Read, Eq)

type Program = Map.Map Int Instruction

type State = (Maybe Int, Int) -- cursor, accumulator

readInstruction :: String -> Instruction
readInstruction line = readInstruction' (words line)
  where
    readInstruction' [(o:operator), ('+':number)] =
      read $ (Char.toUpper o) : operator ++ " " ++ number
    readInstruction' [(o:operator), (negativeNumber)] =
      read $ (Char.toUpper o) : operator ++ negativeNumber

programStep :: State -> Program -> State
programStep (Nothing, accumulator) _ = (Nothing, accumulator)
programStep state@(Just cursor, accumulator) program
  | cursor == lastInstructionId = (Nothing, accumulator')
  | otherwise = state'
  where
    lastInstructionId = Set.findMax . Map.keysSet $ program
    state'@(cursor', accumulator') = nextState state (program Map.! cursor)
    nextState (Nothing, accumulator) _ = (Nothing, accumulator)
    nextState (Just cursor, accumulator) (Acc arg) =
      (Just (cursor + 1), accumulator + arg)
    nextState (Just cursor, accumulator) (Nop arg) =
      (Just (cursor + 1), accumulator)
    nextState (Just cursor, accumulator) (Jmp arg) =
      (Just (cursor + arg), accumulator)

runUntilVisitedAgain :: Set.Set Int -> State -> Program -> Int
runUntilVisitedAgain visited state@(Just cursor, accumulator) program
  | Set.member cursor' visited = accumulator
  | otherwise = runUntilVisitedAgain (Set.insert cursor' visited) state' program
  where
    state'@(Just cursor', accumulator') = programStep state program

part1 :: [String] -> Int
part1 =
  runUntilVisitedAgain Set.empty (Just 1, 0) .
  Map.fromList . zip [1 ..] . List.map readInstruction

----------------------------------------------------
programLastedResult :: Set.Set Int -> State -> Program -> Maybe Int
programLastedResult _ (Nothing, accumulator) _ = Just accumulator
programLastedResult visited state@(Just cursor, accumulator) program
  | isNothing cursor' = Just accumulator
  | isJust cursor' && Set.member (fromJust cursor') visited = Nothing
  | isJust cursor' =
    programLastedResult (Set.insert (fromJust cursor') visited) state' program
  where
    state'@(cursor', accumulator') = programStep state program

alteredPrograms :: [Instruction] -> [[Instruction]]
alteredPrograms [] = []
alteredPrograms ((Jmp arg):is) = ((Nop arg) : is) : (fmap (Jmp arg:) (alteredPrograms is))
alteredPrograms ((Nop arg):is) = ((Jmp arg) : is) : (fmap (Nop arg:) (alteredPrograms is))
alteredPrograms (i:is) = fmap (i:) (alteredPrograms is)

part2 :: [String] -> Int
part2 =
  fromJust .
  head .
  dropWhile (== Nothing) .
  List.map
    (programLastedResult Set.empty (Just 1, 0) . Map.fromList . zip [1 ..]) .
  alteredPrograms . List.map readInstruction

----------------------------------------------------
main :: IO ()
main = do
  str <- readFile "input.txt"
  let input = lines str
  print $ part1 input
  print $ part2 input
