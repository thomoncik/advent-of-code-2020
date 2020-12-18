{-# LANGUAGE FlexibleContexts #-}

module Main (main) where

import Text.Parsec
import Text.Parsec.Expr
import Text.Parsec.Combinator
import Data.Functor
import Data.Function (on)
 
data Exp = Num Int
       | Add Exp Exp
       | Mult Exp Exp
       deriving (Show)
 
expr :: Stream s m Char => ParsecT s u m Exp
expr = buildExpressionParser table factor
  where
    table = [[op "*" Mult AssocLeft, op "+" Add AssocLeft]]
    op s f = Infix (f <$ string s)
    factor = (between `on` char) '(' ')' expr <|> (Num . read <$> many1 digit)

expr2 :: Stream s m Char => ParsecT s u m Exp
expr2 = buildExpressionParser table factor
  where
    table = [[op "+" Add AssocLeft], [op "*" Mult AssocLeft]]
    op s f = Infix (f <$ string s)
    factor = (between `on` char) '(' ')' expr2 <|> (Num . read <$> many1 digit)
 
eval :: Integral a => Exp -> a
eval (Num x) = fromIntegral x
eval (Add a b) = eval a + eval b
eval (Mult a b) = eval a * eval b

part1 = sum . map (either (const (error "Did not parse")) eval . parse expr "")
part2 = sum . map (either (const (error "Did not parse")) eval . parse expr2 "")

main :: IO ()
main = do
  str <- readFile "input.txt"
  let input = lines . filter (/= ' ') $ str
  print $ part1 input
  print $ part2 input
