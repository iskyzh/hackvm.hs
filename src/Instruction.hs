module Instruction where

import Template
import Memory
import Function
import CompiledCode

parseInstruction :: String -> String -> String -> [CompiledCode]
parseInstruction instruction className symbol = let (cmd, args) = break (==' ') instruction in
        (Instruction $ "//" ++ instruction) -- add comments for current processing statement
            : parseInstruction' (trim cmd) (trim args) where
    parseInstruction' :: String -> String -> [CompiledCode]
    parseInstruction' cmd args = case cmd of
        -- arithmetic / logical operations
        -- if only one arg is taken, just modify top
        "not" -> modifyStackTop [Instruction $ "M=!M"]
        "neg" -> modifyStackTop [Instruction $ "M=-M"]
        -- more than one args, pop one and modify one
        "add" -> operatorModifyStackTop [Instruction $ "M=M+D"]
        "sub" -> operatorModifyStackTop [Instruction $ "M=M-D"]
        "and" -> operatorModifyStackTop [Instruction $ "M=D&M"]
        "or" -> operatorModifyStackTop [Instruction $ "M=D|M"]
        -- jump
        "eq" -> conditionModifyStackTop "JEQ" symbol
        "gt" -> conditionModifyStackTop "JGT" symbol
        "lt" -> conditionModifyStackTop "JLT" symbol
        -- memory access operations
        "push" -> parseMemory "push" args
        "pop" -> parseMemory "pop" args
        -- branching commands
        "goto" -> [
                Instruction $ "@" ++ className ++ "." ++ args,
                Instruction $ "0; JMP"
            ]
        "label" -> [
                Instruction $ "(" ++ className ++ "." ++ args ++ ")"
            ]
        "if-goto" -> popFromTop [] [
                Instruction $ "@" ++ className ++ "." ++ args,
                Instruction $ "D; JNE"
            ]
        -- function commands
        "function" -> parseFunction args symbol
        "call" -> parseCall args symbol
        "return" -> parseReturn symbol
        -- otherwise, throw error
        _ -> [CompileError $ "unsupported operation"]
