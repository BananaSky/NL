module Main where

import Parser
import Eval
import Calculus
import Text.Parsec

main = do
  print $ parse parseFunction "" "main:\n\tprint hello world\n\tprint goodbye cruel world"
  print $ parse parseCalculation "" "3*(x^2)+(5*x+9)"
