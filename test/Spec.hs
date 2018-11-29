import Test.Hspec
import Control.Exception (evaluate)
import System.Directory (getCurrentDirectory, withCurrentDirectory, canonicalizePath)
import System.Process
import System.Exit
import Lib (parse, translate, translateMain)
import CompiledCode

compile input output = do 
    vm <- readFile input
    writeFile output $ translateMain vm

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
        {-
        describe "FunctionCalls" $ do
            it "should translate FibonacciElement" $ do
                testSuite "FunctionCalls" "FibonacciElement"

            it "should translate NestedCall" $ do
                testSuite "FunctionCalls" "NestedCall"

            it "should translate SimpleFunction" $ do
                testSuite "FunctionCalls" "SimpleFunction"

            it "should translate StaticsTest" $ do
                testSuite "FunctionCalls" "StaticsTest"
        -}
        describe "ProgramFlow" $ do
            it "should translate BasicLoop" $ do
                testSuite "ProgramFlow" "BasicLoop"

            it "should translate FibonacciSeries" $ do
                testSuite "ProgramFlow" "FibonacciSeries"