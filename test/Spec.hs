import Test.HUnit
import Lib (parse)

main :: IO ()
main = do 
    runTestTT tests
    putStrLn "Test complete"

tests = test [
    "test parse comment" ~: "comment line" ~: [] ~=? parse ["// comment"],
    "test parse comment" ~: "comment line with trailing space" ~: [] ~=? parse ["  // comment"],
    "test parse space" ~: "all space in a line" ~: [] ~=? parse ["    "],
    "test parse arithmetic operations" ~: "add" ~: ["@SP","A=M-1","M=D","@SP","M=M-1","@SP","A=M-1","M=D+M"] ~=? parse ["add"],
    "test parse arithmetic operations" ~: "sub" ~: ["@SP","A=M-1","M=D","@SP","M=M-1","@SP","A=M-1","M=D-M"] ~=? parse ["sub"]
    ]
 