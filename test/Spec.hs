import Test.Hspec
import Control.Exception (evaluate)
import System.Directory (getCurrentDirectory, withCurrentDirectory, canonicalizePath)
import System.Process
import System.Exit
import Lib (parse, translate, translateMain, splitClass)
import CompiledCode

compileFile fileName className = do
    vm <- readFile fileName
    return $ translate vm className

compile input output = do 
    vm <- readFile input
    writeFile output $ translate vm (splitClass input)

bootstrap = [
        "@256",
        "D=A",
        "@SP",
        "M=D",
        "@Sys.init",
        "0; JMP"
    ]

pack package classes output = do 
    code <- mapM (\className -> compileFile (className ++ ".vm") className) classes
    writeFile output $ unlines $ bootstrap ++ code

        
getTestDir path = do
    dir <- getCurrentDirectory
    canonicalizePath (dir ++ "/data/" ++ path ++ "/")

testAndCompare testDir testTraget = do
    homeDir <- getCurrentDirectory
    toolPath <- canonicalizePath (homeDir ++ "/data/tools/CPUEmulator.sh")
    (exitCode, stdout, stderr) <- readCreateProcessWithExitCode (shell $ toolPath ++ " " ++ testDir ++ "/" ++ testTraget) []
    stderr `shouldBe` ""
    exitCode `shouldBe` ExitSuccess

testSuite testSuite testCase = do
    dir <- getTestDir $ testSuite ++ "/" ++ testCase
    withCurrentDirectory dir $ compile (testCase ++ ".vm") (testCase ++ ".asm")
    testAndCompare dir (testCase ++ ".tst")

testPackage testSuite testPackage testClasses = do
    dir <- getTestDir $ testSuite ++ "/" ++ testPackage
    withCurrentDirectory dir $ pack testPackage testClasses (testPackage ++ ".asm")
    testAndCompare dir (testPackage ++ ".tst")
    
main :: IO ()
main = hspec $ do
    describe "parse" $ do
        it "should parse comment line" $ do
            parse ["// comment"] "Spec" `shouldBe` []
  
        it "should parse comment line with trailing space" $
            parse ["  // comment"] "Spec" `shouldBe` []
  
        it "should ignore empty line" $ do
            parse ["    "] "Spec" `shouldBe` []

        it "should error unsupported operation" $ do
            parse ["ed"] "Spec" `shouldContain` [CompileError "unsupported operation"]
        
        it "should error incomplete instruction" $ do
            parse ["ed eq"] "Spec" `shouldContain` [CompileError "unsupported operation"]
                
        it "should error invaild memory segment" $ do
            parse ["push 233"] "Spec" `shouldContain` [CompileError "invaild memory segment"]
                   
    describe "translate" $ do
        describe "StackArithmetic" $ do
            it "should translate SimpleAdd" $  do
                testSuite "StackArithmetic" "SimpleAdd"

            it "should translate StackTest" $  do
                testSuite "StackArithmetic" "StackTest"

        describe "MemoryAccess" $ do
            it "should translate BasicTest" $  do
                testSuite "MemoryAccess" "BasicTest"

            it "should translate PointerTest" $  do
                testSuite "MemoryAccess" "PointerTest"

            it "should translate StaticTest" $  do
                testSuite "MemoryAccess" "StaticTest"

        describe "ProgramFlow" $ do
            it "should translate BasicLoop" $ do
                testSuite "ProgramFlow" "BasicLoop"

            it "should translate FibonacciSeries" $ do
                testSuite "ProgramFlow" "FibonacciSeries"

        describe "FunctionCalls" $ do
            it "should translate SimpleFunction" $ do
                testSuite "FunctionCalls" "SimpleFunction"

            it "should translate NestedCall" $ do
                testPackage "FunctionCalls" "NestedCall" ["Sys"]

            it "should translate FibonacciElement" $ do
                testPackage "FunctionCalls" "FibonacciElement" ["Sys", "Main"]

            it "should translate StaticsTest" $ do
                testPackage "FunctionCalls" "StaticsTest" ["Sys", "Class1", "Class2"]
        