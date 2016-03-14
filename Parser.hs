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
                 try (parseAssignment previous)
             --()<|> try (parsePrint      previous)
             <|> try (parseDerivate   previous)
             <|> try (parsePrevious   previous)
             <|> try parseCalculation
             --()<|> parseType

parsePrevious :: [String] -> Parser Statement
parsePrevious previous = do
  p <- choice (map string previous)
  return $ Identifier p

parseCalculation :: Parser Statement
parseCalculation = do
  expression <- parseCalculation'
  return $ Calculation expression

parseCalculation' :: Parser Expression
parseCalculation' = buildExpressionParser operators terms
  where terms =  parens parseCalculation'
                <|> liftM Variable identifier
                <|> liftM Constant integer

{-
parsePrint :: [String] -> Parser Statement
parsePrint previous = do
  reserved "print"
  optional spaces
  statement <- parseStatement previous
  return $ PrintStatement statement
-}

parseDerivate :: [String] -> Parser Statement
parseDerivate previous = do
  reserved "derivate"
  optional spaces
  expression <- parseStatement previous
  return $ DerivateStatement expression

parseAssignment :: [String] -> Parser Statement
parseAssignment previous = do
 identifier <- many1 letter
 spaces
 char '='
 spaces
 value <- parseStatement previous
 return $ Assignment identifier value

{-
parseFunctionCall :: Parser Statement
parseFunctionCall = do
  identifier <- many1 letter
  char '('
  args <- sepBy1 (many1 letter) (string ", ")
  char ')'
  return $ FunctionCall identifier args-}

{-
parseType :: Parser Statement
parseType = do
    result <- try parseInt <|> parseString
    return $ RawType result
    where parseInt = do
            num <- many1 digit
            return $ Number $ read num
          parseString = do
            sentence <- many1 alphaNum
            return $ Sentence sentence-}
