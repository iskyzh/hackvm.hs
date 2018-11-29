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
    -- constant
    parseMemory' "push" "constant" idx = pushAndModifyStackTop [
            "@" ++ show idx,
            "D=A"            -- save constant to D
        ]
    -- local
    parseMemory' "push" "local" idx = pushMemoryOffset "LCL" idx
    parseMemory' "pop" "local" idx = popMemoryOffset "LCL" idx
    -- argument
    parseMemory' "push" "argument" idx = pushMemoryOffset "ARG" idx
    parseMemory' "pop" "argument" idx = popMemoryOffset "ARG" idx
    -- this
    parseMemory' "push" "this" idx = pushMemoryOffset "THIS" idx
    parseMemory' "pop" "this" idx = popMemoryOffset "THIS" idx
    -- that
    parseMemory' "push" "that" idx = pushMemoryOffset "THAT" idx
    parseMemory' "pop" "that" idx = popMemoryOffset "THAT" idx
    -- temp
    parseMemory' "push" "temp" idx = pushToTop [
            "@5",
            "D=A",
            "@" ++ show idx,
            "A=D+A",             -- access LCL + i
            "D=M"                -- save to D
        ]
    parseMemory' "pop" "temp" idx = popFromTop [
            "@5",
            "D=A",
            "@" ++ show idx,
            "D=D+A",             -- save LCL + i
            "@R13",
            "M=D"                -- to R13
        ] [
            "@R13",
            "A=M",               -- move to LCL + i
            "M=D"                -- save D to LCL + i
        ]
    -- static
    parseMemory' "push" "static" idx = pushToTop [
            "@HACKVM." ++ show idx,
            "D=M"
        ]
    parseMemory' "pop" "static" idx = popFromTop [] [
        "@HACKVM." ++ show idx,
        "M=D"
        ]
    -- pointer
    parseMemory' "push" "pointer" 0 = pushMemoryPointer "THIS"
    parseMemory' "push" "pointer" 1 = pushMemoryPointer "THAT"
    parseMemory' "pop" "pointer" 0 = popMemoryPointer "THIS"
    parseMemory' "pop" "pointer" 1 = popMemoryPointer "THAT"
    -- otherwise
    parseMemory' _ _ _ = error ("compile error")

    pushMemoryPointer :: String -> [String]
    pushMemoryPointer target = pushToTop [
            "@" ++ target,
            "D=M"
        ]

    popMemoryPointer :: String -> [String]
    popMemoryPointer target = popFromTop [] [
            "@" ++ target,
            "M=D"
        ]

    popMemoryOffset :: String -> Int -> [String]
    popMemoryOffset label idx = popFromTop [
            "@" ++ label,
            "D=M",
            "@" ++ show idx,
            "D=D+A",             -- save LCL + i
            "@R13",
            "M=D"                -- to R13
        ] [
            "@R13",
            "A=M",               -- move to LCL + i
            "M=D"                -- save D to LCL + i
        ]

    pushMemoryOffset :: String -> Int -> [String]
    pushMemoryOffset label idx = pushToTop [
            "@" ++ label,
            "D=M",
            "@" ++ show idx,
            "A=D+A",             -- access LCL + i
            "D=M"                -- save to D
        ]

pushToTop cmd = cmd ++ [
        "@SP",
        "A=M",      -- move to stack top
        "M=D",      -- save D
        "@SP",
        "M=M+1"     -- push stack top
    ]

popFromTop before after = before ++ [
        "@SP",
        "AM=M-1",     -- pop and move to previous stack top
        "D=M"         -- save stack top to D
    ] ++ after

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

pushAndModifyStackTop cmd = cmd ++ [
        "@SP",
        "M=M+1",    -- push
        "A=M-1",    -- move to stack top
        "M=D"       -- save data
    ]