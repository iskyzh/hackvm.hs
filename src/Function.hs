module Function where

import Template
import CompiledCode

moveForwardSP 0 = []
moveForwardSP n = [
        Instruction $ "A=M",
        Instruction $ "M=0",
        Instruction $ "@SP",
        Instruction $ "M=M+1"
    ] ++ moveForwardSP (n - 1)

parseFunction :: String -> String -> [CompiledCode]
parseFunction args symbol = let 
    (functionName, _args) = break (== ' ') args
    nVars = (read _args)::Int
    in [
        Instruction $ "(" ++ functionName ++ ")",
        Instruction $ "@" ++ show nVars,
        Instruction $ "@SP"
    ] ++ moveForwardSP nVars

parseCall :: String -> String -> [CompiledCode]
parseCall args symbol = let 
        (functionName, _args) = break (== ' ') args
        nArgs = (read _args)::Int
        retAddrLabel = symbol ++ "$ret"
        in
    pushToTop [
        Instruction $ "@" ++ retAddrLabel,
        Instruction $ "D=A"    -- save return addr to D
    ] ++                       -- push to top
    pushToTop [
        Instruction $ "@LCL",
        Instruction $ "D=M"    -- save LCL
    ] ++                       -- push to top
    pushToTop [
        Instruction $ "@ARG",
        Instruction $ "D=M"    -- save ARG
    ] ++                       -- push to top
    pushToTop [
        Instruction $ "@THIS",
        Instruction $ "D=M"    -- save THIS
    ] ++                       -- push to top
    pushToTop [
        Instruction $ "@THAT",
        Instruction $ "D=M"    -- save THAT
    ] ++                       -- push to top
    [
        Instruction $ "@" ++ show (5 + nArgs),
        Instruction $ "D=A",   -- save (5 + nArgs) to D
        Instruction $ "@SP",
        Instruction $ "D=M-D", -- save (SP - 5 - nArgs) to D
        Instruction $ "@ARG",
        Instruction $ "M=D",   -- reposition ARG,
        Instruction $ "@SP",
        Instruction $ "D=M",   -- save SP to D
        Instruction $ "@LCL",
        Instruction $ "M=D",   -- save D to LCL
        Instruction $ "@" ++ functionName,
        Instruction $ "0; JMP",-- goto function
        Instruction $ "(" ++ retAddrLabel ++ ")"
    ]

parseReturn :: String -> [CompiledCode]
parseReturn symbol = [
        Instruction $ "@5",
        Instruction $ "D=A",
        Instruction $ "@LCL",
        Instruction $ "A=M-D",  -- move to LCL - 5
        Instruction $ "D=M",    -- save return address to D
        Instruction $ "@R13",
        Instruction $ "M=D",    -- save D to R13
        Instruction $ "@SP",
        Instruction $ "A=M-1",  -- move to stack top
        Instruction $ "D=M",    -- save return value to D
        Instruction $ "@ARG",
        Instruction $ "A=M",
        Instruction $ "M=D",    -- save D to *ARG
        Instruction $ "@ARG",
        Instruction $ "D=M",    -- save ARG to D
        Instruction $ "@SP",
        Instruction $ "M=D+1"   -- SP = ARG + 1
    ] ++ [
        Instruction $ "@LCL",
        Instruction $ "AM=M-1",
        Instruction $ "D=M",
        Instruction $ "@THAT",
        Instruction $ "M=D"     -- save endframe - 1 to THAT
    ] ++ [
        Instruction $ "@LCL",
        Instruction $ "AM=M-1",
        Instruction $ "D=M",
        Instruction $ "@THIS",
        Instruction $ "M=D"     -- save endframe - 2 to THIS
    ] ++ [
        Instruction $ "@LCL",
        Instruction $ "AM=M-1",
        Instruction $ "D=M",
        Instruction $ "@ARG",
        Instruction $ "M=D"     -- save endframe - 3 to ARG
    ] ++ [
        Instruction $ "@LCL",
        Instruction $ "AM=M-1",
        Instruction $ "D=M",
        Instruction $ "@LCL",
        Instruction $ "M=D"     -- save endframe - 4 to LCL
    ] ++ [
        Instruction $ "@R13",
        Instruction $ "A=M",
        Instruction $ "0; JMP"
    ]