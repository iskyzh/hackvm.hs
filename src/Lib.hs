module Lib
    (
        translate,
        parse,
        parseInstruction
    ) where

import Data.Char (isSpace)
import Data.List (isPrefixOf)
import SymbolGenerator (symbols)
import Template
import Memory
import Instruction

translate :: String -> String
translate = unlines . parse . lines

parse :: [String] -> [String]
parse instructions = parse' instructions (symbols "test") where
    parse' :: [String] -> [String] -> [String]
    parse' (instruction:xs) symbols
        | "//" `isPrefixOf` (trim instruction) = [] ++ parse' xs symbols -- ignore comments
        | all isSpace instruction = [] ++ parse' xs symbols -- ignore empty line
        | otherwise = parseInstruction instruction (head symbols) ++ parse' xs (tail symbols)
    parse' _ _ = []
