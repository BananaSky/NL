module Statement(parseStatement) where

import           Text.Parsec

import Calc
import Conditional
import Assignment

parseStatement = try parseAssignment
             <|> parseIfElse

main = print $ parse parseStatement "" "x = 9 * 3"
