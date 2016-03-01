module Assignment(parseAssignment) where

import Syntax
import Calc

import           Text.Parsec
import           Text.Parsec.String

--Assignment

parseAssignment :: Parser Statement
parseAssignment = do
  identifier <- many1 letter
  space
  (char '=')
  space
  value <- parseExpression
  return $ AssignmentStatement $ Assignment identifier value
