module Main where

import Parser
import Text.Parsec

main = print $ parse parseFunction "" "main:\n\tprint hello world\n\tprint goodbye cruel world"
