module Parser where

import Control.Monad

import Text.Parsec
import Text.Parsec.String
import Text.Parsec.Prim
import Text.ParserCombinators.Parsec.Expr

import Syntax
import Base

parseStatement = try parseAssignment
             <|> try parsePrint
             <|> try parseIfElse
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
  statement <- parseStatement
  return $ PrintStatement statement

parseReturn :: Parser Statement
parseReturn = do
  reserved "return"
  optional spaces
  statement <- parseStatement
  return $ ReturnStatement statement

parseDerivate :: Parser Statement
parseDerivate = do
  reserved "derivate"
  optional spaces
  expression <- parseCalculation
  return $ DerivateStatement expression

parseIfElse :: Parser Statement
parseIfElse = do
 reserved "if"
 condition <- parseConditional
 reserved "then"
 thenBranch <- many1 parseStatement
 reserved "else"
 elseBranch <- many1 parseStatement
 return $ IfElseStatement $ IfElse condition thenBranch elseBranch

parseConditional :: Parser Conditional
parseConditional = buildExpressionParser logicOperators boolean
  where boolean =  parens parseConditional
                   <|> (reserved "true"  >> return (Boolean True ))
                   <|> (reserved "false" >> return (Boolean False))
                   <|> relationExpression

relationExpression = do
   a1 <- parseCalculation
   op <- relation
   a2 <- parseCalculation
   return $ Relation op a1 a2
   where relation =   (reservedOp ">" >> return Greater)
                  <|> (reservedOp "<" >> return Less)
                  <|> (reservedOp ">=" >> return GreaterOrEqual)
                  <|> (reservedOp "<=" >> return LessOrEqual)
                  <|> (reservedOp "==" >> return Equal)

parseAssignment :: Parser Statement
parseAssignment = do
 identifier <- many1 letter
 spaces
 char '='
 spaces
 value <- parseCalculation
 return $ AssignmentStatement $ Assignment identifier value

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
