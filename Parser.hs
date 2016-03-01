module Parser where

import Control.Monad

import Text.Parsec
import Text.Parsec.String
import Text.Parsec.Prim
import Text.ParserCombinators.Parsec.Expr

import Syntax

parseStatement = try parseFunction
             <|> try parseAssignment
             <|> try parsePrint
             <|> try parseIfElse
             <|> parseString

parseCalculation :: Parser Expression
parseCalculation = buildExpressionParser operators terms
  where terms =  parens parseCalculation
                <|> liftM Variable identifier
                <|> liftM Constant integer

parseString :: Parser Statement
parseString = do
  s <- many1 $ noneOf "\n\r"
  return $ RawString s

parsePrint :: Parser Statement
parsePrint = do
  reserved "print"
  statement <- parseStatement
  return $ PrintStatement statement

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
 space
 (char '=')
 space
 value <- parseCalculation
 return $ AssignmentStatement $ Assignment identifier value

parseFunction :: Parser Statement
parseFunction = do
  name <- (many $ noneOf " :")
  args <- functionArgs
  char ':'
  eol
  cont <- many1 functionBody
  return $ FunctionStatement $ Function name args cont
    where functionBody = do
                tab
                statement <- parseStatement
                eol
                return statement
          functionArgs = do
            try (parseArgs)
            <|> return []
            where parseArgs = do
                  space
                  sepBy (many $ noneOf " :") (char ' ') >>= return


eol =   try (string "\n\r")
    <|> try (string "\r\n")
    <|> string "\n"
    <|> string "\r"
    <?> "end of line"
