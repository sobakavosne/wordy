{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module WordProblem
  ( answer
  ) where

import           Data.Maybe         (fromMaybe)
import           Text.Parsec        (char, digit, many1, option, parse, sepBy1,
                                     spaces, string, try, (<|>))
import           Text.Parsec.String (Parser)

arithmeticPrefix :: String
arithmeticPrefix = "What is"

data Operation
  = Plus
  | Minus
  | Multiply
  | Divide
  deriving (Show)

data ExprElement
  = Num Integer
  | Op Operation
  deriving (Show)

integer :: Parser Integer
integer = do
  neg <- option 1 (char '-' >> return (-1))
  num <- read <$> many1 digit
  return (neg * num)

operation :: Parser Operation
operation =
  try (string "plus" >> return Plus) <|> try (string "minus" >> return Minus) <|>
  try (string "multiplied by" >> return Multiply) <|>
  try (string "divided by" >> return Divide)

isOperation :: String -> Bool
isOperation input =
  case parse operation "" input of
    Left _  -> False
    Right _ -> True

exprElement :: Parser ExprElement
exprElement = (Num <$> integer) <|> (Op <$> operation)

exprList :: Parser [ExprElement]
exprList = exprElement `sepBy1` spaces

evalExpr :: [ExprElement] -> Maybe Integer
evalExpr (Num x:rest) = evalRest x rest
  where
    evalRest acc []               = Just acc
    evalRest acc (Op op:Num y:xs) = evalRest (applyOperation op acc y) xs
    evalRest _ _                  = Nothing

applyOperation :: Operation -> Integer -> Integer -> Integer
applyOperation Plus     = (+)
applyOperation Minus    = (-)
applyOperation Multiply = (*)
applyOperation Divide   = div

parseArithmeticExpr :: String -> Maybe [ExprElement]
parseArithmeticExpr input =
  case parse exprList "" input of
    Left _       -> Nothing
    Right result -> Just result

parseAndEval :: String -> Maybe Integer
parseAndEval input = evalExpr =<< parseArithmeticExpr input

maybeMathProblem :: [String] -> Maybe [String]
maybeMathProblem (x:y:zs)
  | isProblem = Just problem
  | otherwise = Nothing
  where
    isProblem :: Bool
    isProblem =
      unwords [x, y] == arithmeticPrefix &&
      (last . last) zs == '?' && (not . isOperation . head) zs
    problem :: [String]
    problem = init zs ++ [(init . last) zs]

answer :: String -> Maybe Integer
answer = parseAndEval . unwords . fromMaybe [] . maybeMathProblem . words
