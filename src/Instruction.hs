module Instruction where

import Template
import Memory
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
