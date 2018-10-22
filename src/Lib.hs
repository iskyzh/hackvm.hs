module Lib
    (
        translate,
        parse,
        parseInstruction
    ) where

import Data.Char (isSpace)
import Data.List (isPrefixOf, dropWhileEnd)

trim = dropWhileEnd isSpace . dropWhile isSpace

translate :: String -> String
translate = unlines . parse . lines

parse :: [String] -> [String]
parse instructions = parse' instructions 0 where
    parse' :: [String] -> Int -> [String]
    parse' (instruction:xs) cnt
        | "//" `isPrefixOf` (trim instruction) = [] ++ parse' xs cnt -- ignore comments
        | all isSpace instruction = [] ++ parse' xs cnt -- ignore empty line
        | otherwise = parseInstruction instruction cnt ++ parse' xs (cnt + 1)
    parse' _ _ = []

parseInstruction :: String -> Int -> [String]
parseInstruction instruction cnt = let (cmd, args) = break (==' ') instruction in
    ("//" ++ instruction) : parseInstruction' (trim cmd) (trim args) cnt where
    parseInstruction' :: String -> String -> Int -> [String]
    parseInstruction' cmd args cnt = case cmd of
        -- arithmetic operations
        -- if only one arg is taken, just modify top
        "not" -> stackTop ++ ["M=!M"]
        "neg" -> stackTop ++ ["M=-M"]
        -- more than one args, pop one and modify one
        "add" -> takePopD ++ ["M=M+D"]
        "sub" -> takePopD ++ ["M=M-D"]
        "and" -> takePopD ++ ["M=D&M"]
        "or" -> takePopD ++ ["M=D|M"]
        -- jump
        "eq" -> takePopD ++ doCmp cnt "JEQ"
        "gt" -> takePopD ++ doCmp cnt "JGT"
        "lt" -> takePopD ++ doCmp cnt "JLT"
        -- access operations
        "push" -> parseMemory "push" args
        "pop" -> parseMemory "pop" args

parseMemory :: String -> String -> [String]
parseMemory cmd args = let (src, idx) = break (==' ') args in
    parseMemory' cmd src (read (trim idx)::Int) where
    parseMemory' :: String -> String -> Int -> [String]
    parseMemory' "push" "constant" idx = ["@" ++ show idx, "D=A"] ++ stackPush ++ ["M=D"]

stackTop = ["@SP", "A=M-1"]
saveD = ["D=M"]
stackPop = ["@SP", "M=M-1"]
stackTopPop = ["@SP", "AM=M-1"]
stackPush = ["@SP", "M=M+1", "A=M-1"]
takePopD = stackTop ++ saveD ++ stackPop ++ stackTop
doCmp cnt cmd = ["D=M-D"] ++ ["M=-1", "@cmp" ++ show cnt, "D; " ++ cmd] ++ stackTop ++ ["M=0", "(cmp" ++ show cnt ++ ")"]
