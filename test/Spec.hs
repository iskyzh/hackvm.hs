import Test.Hspec
import Control.Exception (evaluate)
import System.Directory (getCurrentDirectory, withCurrentDirectory, canonicalizePath)
import System.Process
import System.Exit
import Lib (parse, translate)

compile input output = do 
    vm <- readFile input
    writeFile output $ translate vm

getTestDir path = do
    dir <- getCurrentDirectory
    canonicalizePath (dir ++ "/data/" ++ path ++ "/")

testAndCompare testDir testTraget = do
    home_dir <- getCurrentDirectory
    toolPath <- canonicalizePath (home_dir ++ "/data/tools/CPUEmulator.sh")
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
            parse ["// comment"] `shouldBe` []
  
        it "should parse comment line with trailing space" $
            parse ["  // comment"] `shouldBe` []
  
        it "should ignore empty line" $ do
            parse ["    "] `shouldBe` []
            
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