module Memory where

import           Template
import           CompiledCode

parseMemory :: String -> String -> String -> [CompiledCode]

parseMemory cmd args className =
    let (src, idx) = break (== ' ') args
    in  parseMemory' cmd src (read (trim idx) :: Int)  where
    parseMemory' :: String -> String -> Int -> [CompiledCode]
    -- constant
    parseMemory' "push" "constant" idx = pushAndModifyStackTop
        [ Instruction $ "@" ++ show idx
        , Instruction $ "D=A"            -- save constant to D
        ]
    -- local
    parseMemory' "push" "local"    idx = pushMemoryOffset "LCL" idx
    parseMemory' "pop"  "local"    idx = popMemoryOffset "LCL" idx
    -- argument
    parseMemory' "push" "argument" idx = pushMemoryOffset "ARG" idx
    parseMemory' "pop"  "argument" idx = popMemoryOffset "ARG" idx
    -- this
    parseMemory' "push" "this"     idx = pushMemoryOffset "THIS" idx
    parseMemory' "pop"  "this"     idx = popMemoryOffset "THIS" idx
    -- that
    parseMemory' "push" "that"     idx = pushMemoryOffset "THAT" idx
    parseMemory' "pop"  "that"     idx = popMemoryOffset "THAT" idx
    -- temp
    parseMemory' "push" "temp"     idx = pushToTop
        [ Instruction $ "@5"
        , Instruction $ "D=A"
        , Instruction $ "@" ++ show idx
        , Instruction $ "A=D+A"
        ,             -- access LCL + i
          Instruction $ "D=M"                -- save to D
        ]
    parseMemory' "pop" "temp" idx = popFromTop
        [ Instruction $ "@5"
        , Instruction $ "D=A"
        , Instruction $ "@" ++ show idx
        , Instruction $ "D=D+A"
        ,             -- save LCL + i
          Instruction $ "@R13"
        , Instruction $ "M=D"                -- to R13
        ]
        [ Instruction $ "@R13"
        , Instruction $ "A=M"
        ,               -- move to LCL + i
          Instruction $ "M=D"                -- save D to LCL + i
        ]
    -- static
    parseMemory' "push" "static" idx =
        pushToTop [Instruction $ staticAddr idx, Instruction $ "D=M"]
    parseMemory' "pop" "static" idx =
        popFromTop [] [Instruction $ staticAddr idx, Instruction $ "M=D"]
    -- pointer
    parseMemory' "push" "pointer" 0 = pushMemoryPointer "THIS"
    parseMemory' "push" "pointer" 1 = pushMemoryPointer "THAT"
    parseMemory' "pop"  "pointer" 0 = popMemoryPointer "THIS"
    parseMemory' "pop"  "pointer" 1 = popMemoryPointer "THAT"
    -- otherwise
    parseMemory' _      _         _ = [CompileError "invaild memory segment"]

    pushMemoryPointer :: String -> [CompiledCode]
    pushMemoryPointer target =
        pushToTop [Instruction $ "@" ++ target, Instruction $ "D=M"]

    popMemoryPointer :: String -> [CompiledCode]
    popMemoryPointer target =
        popFromTop [] [Instruction $ "@" ++ target, Instruction $ "M=D"]

    popMemoryOffset :: String -> Int -> [CompiledCode]
    popMemoryOffset label idx = popFromTop
        [ Instruction $ "@" ++ label
        , Instruction $ "D=M"
        , Instruction $ "@" ++ show idx
        , Instruction $ "D=D+A"
        ,             -- save LCL + i
          Instruction $ "@R13"
        , Instruction $ "M=D"                -- to R13
        ]
        [ Instruction $ "@R13"
        , Instruction $ "A=M"
        ,               -- move to LCL + i
          Instruction $ "M=D"                -- save D to LCL + i
        ]

    pushMemoryOffset :: String -> Int -> [CompiledCode]
    pushMemoryOffset label idx = pushToTop
        [ Instruction $ "@" ++ label
        , Instruction $ "D=M"
        , Instruction $ "@" ++ show idx
        , Instruction $ "A=D+A"
        ,             -- access LCL + i
          Instruction $ "D=M"                -- save to D
        ]
    staticAddr idx = "@" ++ className ++ ".STATIC." ++ show idx

