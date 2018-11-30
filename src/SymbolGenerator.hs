module SymbolGenerator
    ( symbols
    )
where


symbols :: String -> [String]
symbols name = map (\num -> name ++ "." ++ show num) [0 ..]
