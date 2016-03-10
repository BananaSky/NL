module Parser where

import Control.Monad

import Text.Parsec
import Text.Parsec.String
import Text.Parsec.Prim
import Text.ParserCombinators.Parsec.Expr

import Syntax
import Base

parseFile :: [String] -> [Statement] -> [Statement]
parseFile [] _ = []
parseFile (s:ss) previous = statement : (parseFile ss $ previous ++ [statement])
  where statement = case parse (parseStatement previous) "" s of
              Right s -> s
              Left _ -> undefined

parseStatement :: [Statement] -> Parser Statement
parseStatement previous =
                 try parseAssignment
             <|> try parsePrint
             <|> try parseDerivate
             <|> parseType

parseCalculation :: Parser Expression
parseCalculation = buildExpressionParser operators terms
  where terms =  parens parseCalculation
                <|> liftM Variable identifier
                <|> liftM Constant integer

parsePrint :: Parser Statement
parsePrint = do
  reserved "print"
  optional spaces
  statement <- parseStatement []
  return $ PrintStatement statement

parseDerivate :: Parser Statement
parseDerivate = do
  reserved "derivate"
  optional spaces
  expression <- parseCalculation
  return $ DerivateStatement expression

parseAssignment :: Parser Statement
parseAssignment = do
 identifier <- many1 letter
 spaces
 char '='
 spaces
 value <- parseCalculation
 return $ Assignment identifier value

parseFunctionCall :: Parser Statement
parseFunctionCall = do
  identifier <- many1 letter
  char '('
  args <- sepBy1 (many1 letter) (string ", ")
  char ')'
  return $ FunctionCall identifier args

parseType :: Parser Statement
parseType = do
    result <- try parseInt <|> parseString
    return $ RawType result
    where parseInt = do
            num <- many1 digit
            return $ Number $ read num
          parseString = do
            sentence <- many1 alphaNum
            return $ Sentence sentence
