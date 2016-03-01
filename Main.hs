module Main where

import Parser
import Text.Parsec

main = print $ parse parseStatement "" "if (x < 2) then y = 6 else y = 8 z = 9"
