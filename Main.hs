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
    content <- hGetContents handle
    let contents = lines content
    let statements = parseFile contents []

    mapM printLn statements
    hClose handle


    eval statements statements

    return ()
