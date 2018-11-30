module CompiledCode where

data CompiledCode = Instruction String | CompileError String
    deriving(Show, Eq)
