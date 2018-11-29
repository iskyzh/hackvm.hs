module Template where

import Data.Char (isSpace)
import Data.List (isPrefixOf, dropWhileEnd)

import CompiledCode

pushToTop cmd = cmd ++ [
        Instruction $ "@SP",
        Instruction $ "A=M",      -- move to stack top
        Instruction $ "M=D",      -- save D
        Instruction $ "@SP",
        Instruction $ "M=M+1"     -- push stack top
    ]

popFromTop before after = before ++ [
        Instruction $ "@SP",
        Instruction $ "AM=M-1",     -- pop and move to previous stack top
        Instruction $ "D=M"         -- save stack top to D
    ] ++ after

modifyStackTop cmd = [
        Instruction $ "@SP", 
        Instruction $ "A=M-1" -- access stack top
    ] ++ cmd

operatorModifyStackTop cmd = [
        Instruction $ "@SP",
        Instruction $ "AM=M-1", -- pop one from stack top and access last top
        Instruction $ "D=M",    -- save first operate element to D
        Instruction $ "A=A-1"   -- move to the second element
    ] ++ cmd

conditionModifyStackTop jmp symbol = let cmp = "cmp." ++ symbol in
    operatorModifyStackTop [
        -- save first element to D and access the second
        Instruction $ "D=M-D",     -- compare 2-1
        Instruction $ "M=-1",      -- top element is true at first
        Instruction $ '@':cmp,     -- specify jump point
        Instruction $ "D; " ++ jmp -- jump
    ] ++ modifyStackTop [
                     -- go to stack top again
        Instruction $ "M=0",       -- set stack top to false is condition is not satisfied
        Instruction $ "(" ++ cmp ++ ")"
    ]

pushAndModifyStackTop cmd = cmd ++ [
        Instruction $ "@SP",
        Instruction $ "M=M+1",    -- push
        Instruction $ "A=M-1",    -- move to stack top
        Instruction $ "M=D"       -- save data
    ]

trim = dropWhileEnd isSpace . dropWhile isSpace
