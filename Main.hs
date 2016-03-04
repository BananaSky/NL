module Main where

import Parser
import Eval
import Base
import Syntax
import Text.Parsec
import System.IO

main :: IO ()
main = do
    handle <- openFile "test.nl" ReadMode
    contents <- hGetContents handle
    print $ parse parseFile "" contents
    hClose handle
