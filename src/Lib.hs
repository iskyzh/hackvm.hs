module Lib
    (
        translate,
        parse,
        parseInstruction
    ) where

import Data.Char (isSpace)
import Data.List (isPrefixOf, dropWhileEnd)
import SymbolGenerator (symbols)

trim = dropWhileEnd isSpace . dropWhile isSpace

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

parseInstruction :: String -> String -> [String]
parseInstruction instruction symbol = let (cmd, args) = break (==' ') instruction in
    ("//" ++ instruction) -- add comments for current processing statement
        : parseInstruction' (trim cmd) (trim args) where
    parseInstruction' :: String -> String -> [String]
    parseInstruction' cmd args = case cmd of
        -- arithmetic operations
        -- if only one arg is taken, just modify top
        "not" -> modifyStackTop ["M=!M"]
        "neg" -> modifyStackTop ["M=-M"]
        -- more than one args, pop one and modify one
        "add" -> operatorModifyStackTop ["M=M+D"]
        "sub" -> operatorModifyStackTop ["M=M-D"]
        "and" -> operatorModifyStackTop ["M=D&M"]
        "or" -> operatorModifyStackTop ["M=D|M"]
        -- jump
        "eq" -> conditionModifyStackTop "JEQ" symbol
        "gt" -> conditionModifyStackTop "JGT" symbol
        "lt" -> conditionModifyStackTop "JLT" symbol
        -- access operations
        "push" -> parseMemory "push" args
        "pop" -> parseMemory "pop" args
        -- otherwise, throw error
        _ -> error ("compile error: " ++ instruction)


parseMemory :: String -> String -> [String]
parseMemory cmd args = let (src, idx) = break (==' ') args in
    parseMemory' cmd src (read (trim idx)::Int) where
    parseMemory' :: String -> String -> Int -> [String]
    parseMemory' "push" "constant" idx = ["@" ++ show idx, "D=A", "@SP", "M=M+1", "A=M-1", "M=D"]


modifyStackTop cmd = [
    "@SP", 
    "A=M-1" -- access stack top
    ] ++ cmd

operatorModifyStackTop cmd = [
    "@SP",
    "AM=M-1", -- pop one from stack top and access last top
    "D=M",    -- save first operate element to D
    "A=A-1"   -- move to the second element
    ] ++ cmd

conditionModifyStackTop jmp symbol = let cmp = "cmp." ++ symbol in
    operatorModifyStackTop [
                     -- save first element to D and access the second
        "D=M-D",     -- compare 2-1
        "M=-1",      -- top element is true at first
        '@':cmp,     -- specify jump point
        "D; " ++ jmp -- jump
    ] ++ modifyStackTop [
                     -- go to stack top again
        "M=0",       -- set stack top to false is condition is not satisfied
        "(" ++ cmp ++ ")"
    ]
