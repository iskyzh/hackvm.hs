module Main where

import Lib

import System.Environment (getArgs)

interactWith function inputFile outputFile = do
    input <- readFile inputFile
    writeFile outputFile (function input)

main :: IO ()
main = mainWith translateMain
    where 
        mainWith function = do
            args <- getArgs
            case args of
                [input, output] -> interactWith function input output
                _ -> putStrLn "err: exactly two arguments needed"
                