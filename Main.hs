module Main where

import Parser
import Text.Parsec

main = print $ parse parseFunction "" "main x:\n\tprint hello world\n\tprint goodbye world\n"
