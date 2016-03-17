module Parser where

import Control.Monad

import Text.Parsec
import Text.Parsec.String
import Text.Parsec.Prim
import Text.ParserCombinators.Parsec.Expr

import Syntax
import Base

parseFile :: [String] -> [String] -> [Statement]
parseFile [] _ = []
parseFile (s:ss) previous = if statement == None
                            then (parseFile ss $ fromStatement previous statement)
                            else statement : (parseFile ss $ fromStatement previous statement)
  where statement = case parse (parseStatement previous) "" s of
              Right s -> s
              Left _ -> None

fromStatement :: [String] -> Statement -> [String]
fromStatement previous (Assignment s _) = previous ++ [s]
fromStatement previous _ = previous

parseStatement :: [String] -> Parser Statement
parseStatement previous =
                 try (parseAssignment        previous)
             <|> try (parseFunctionCall      previous)
             <|> try (parseDerivate          previous)
             <|> try (parseIntegrate         previous)
             <|> try (parsePrevious          previous)
             <|> try parseCalculation

parsePrevious :: [String] -> Parser Statement
parsePrevious previous = do
  p <- choice (map string $ reverse previous)
  return $ Identifier p

parseCalculation :: Parser Statement
parseCalculation  = do
  expression <- parseCalculation'
  return $ Calculation expression

parseCalculation' :: Parser Expression
parseCalculation' = buildExpressionParser operators terms
  where terms =  parens parseCalculation'
                <|> liftM Variable identifier
                <|> liftM Constant integer

parseDerivate :: [String] -> Parser Statement
parseDerivate previous = do
  reserved "derivate"
  optional spaces
  expression <- parseStatement previous
  return $ DerivateStatement expression

parseIntegrate :: [String] -> Parser Statement
parseIntegrate previous = do
  reserved "integrate"
  optional spaces
  expression <- parseStatement previous
  return $ IntegrateStatement expression

parseAssignment :: [String] -> Parser Statement
parseAssignment previous = do
 identifier <- many1 letter
 spaces
 char '='
 spaces
 value <- parseStatement previous
 return $ Assignment identifier value

parseFunctionCall :: [String] -> Parser Statement
parseFunctionCall previous = do
  identifier <- parsePrevious previous
  char '('
  args <- sepBy1 integer (string ", ")
  char ')'
  return $ FunctionCall identifier args
