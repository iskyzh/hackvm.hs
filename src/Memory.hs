module Memory where

import Template

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
