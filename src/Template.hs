module Template where
    
import Data.Char (isSpace)
import Data.List (isPrefixOf, dropWhileEnd)

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

trim = dropWhileEnd isSpace . dropWhile isSpace
