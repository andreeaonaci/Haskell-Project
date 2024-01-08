module Test.Util where

import Control.Monad.State (State, evalState, execState, runState)
import Data.Maybe (listToMaybe)
import Parser
import Result (Result (..), isError)
import System.Environment (getArgs)
import qualified Test.Data  as Data
import Test.SimpleTest
import Test.SimpleTest.Expectation
import Test.SimpleTest.Mock (makeMockIOState)
import qualified Test.SimpleTest.Mock  as Mock
import Test.SimpleTest.TestCase
import Text.Printf (printf)

mkParseShouldFailWith :: (Show a, Eq a) => Parser a -> String -> ParseError -> EqualityAssertion
mkParseShouldFailWith parser str err = parse parser str `shouldBe` Error err

mkParseShouldFail :: (Show a) => Parser a -> String -> PredicateAssertion
mkParseShouldFail parser str = isError `shouldHold` parse parser str

mkShouldParseAs :: (Show a, Eq a) => Parser a -> String -> a -> EqualityAssertion
mkShouldParseAs parser str v = parse parser str `shouldBe` Success v

mkShouldParseAsWithRest :: (Show a, Eq a) => Parser a -> String -> (a, String) -> EqualityAssertion
mkShouldParseAsWithRest parser input (v, remaining) = runParser parser input `shouldBe` Success (v, remaining)

shouldHaveEffects :: String -> Mock.MockIOState -> State Mock.MockIOState a -> (Mock.MockIOState -> Bool) -> PredicateAssertion
shouldHaveEffects msg init fn pred = pred `shouldHold` execState fn init `withMessage` msg

shouldReadFromFile :: FilePath -> Mock.MockIOState -> State Mock.MockIOState a -> PredicateAssertion
shouldReadFromFile file init fn = Mock.fileWasRead file `shouldHold` (execState fn init) `withMessage` (printf "\"%s\" was read" file)

shouldReadStdin :: Mock.MockIOState -> State Mock.MockIOState a -> PredicateAssertion
shouldReadStdin init fn = Mock.stdinWasRead `shouldHold` (execState fn init) `withMessage` "stdin was read"

shouldCheckIfFileExists :: FilePath -> Mock.MockIOState -> State Mock.MockIOState a -> PredicateAssertion
shouldCheckIfFileExists file init fn = Mock.fileWasChecked file `shouldHold` (execState fn init) `withMessage` (printf "\"%s\" was checked for existence" file)

shouldNotWriteFile :: FilePath -> Mock.MockIOState -> State Mock.MockIOState a -> PredicateAssertion
shouldNotWriteFile file init fn = (not . Mock.fileWasWritten file) `shouldHold` (execState fn init) `withMessage` (printf "\"%s\" wasn't written" file)

shouldNotReadFile :: FilePath -> Mock.MockIOState -> State Mock.MockIOState a -> PredicateAssertion
shouldNotReadFile file init fn = (not . Mock.fileWasRead file) `shouldHold` (execState fn init) `withMessage` (printf "\"%s\" wasn't read" file)

shouldNotTouchFile :: FilePath -> Mock.MockIOState -> State Mock.MockIOState a -> PredicateAssertion
shouldNotTouchFile file init fn = (not . Mock.fileWasTouched file) `shouldHold` (execState fn init) `withMessage` (printf "\"%s\" wasn't checked, read or written" file)

shouldWriteToStdout :: String -> Mock.MockIOState -> State Mock.MockIOState a -> PredicateAssertion
shouldWriteToStdout msg init fn = Mock.stdoutContains msg `shouldHold` (execState fn init) `withMessage` (printf "stdout contains %s" (show msg))

shouldWriteAllToStdout :: [String] -> Mock.MockIOState -> State Mock.MockIOState a -> PredicateAssertion
shouldWriteAllToStdout msgs init fn = (\a -> Prelude.and $ (map Mock.stdoutContains msgs) <*> pure a) `shouldHold` (execState fn init) `withMessage` (printf "stdout contains %s" (unlines $ map show msgs))

shouldNotWriteToStdout :: Mock.MockIOState -> State Mock.MockIOState a -> PredicateAssertion
shouldNotWriteToStdout init fn = Mock.sameStdout init `shouldHold` (execState fn init) `withMessage` "stdout is not written to"

shouldWriteFile :: FilePath -> Mock.MockIOState -> State Mock.MockIOState a -> PredicateAssertion
shouldWriteFile file init fn = Mock.fileWasWritten file `shouldHold` (execState fn init) `withMessage` (printf "\"%s\" was written" file)

fileShouldContain :: FilePath -> String -> Mock.MockIOState -> State Mock.MockIOState a -> PredicateAssertion
fileShouldContain file contents init fn = Mock.fileContentsContains contents file `shouldHold` (execState fn init) `withMessage` (printf "\"%s\" contains \"%s\"" file contents)

shouldHaveResult :: (Show a, Eq a) => Mock.MockIOState -> State Mock.MockIOState a -> a -> EqualityAssertion
shouldHaveResult init fn a = (evalState fn init) `shouldBe` a

resultShouldSatisfy :: (Show a) => Mock.MockIOState -> State Mock.MockIOState a -> (a -> Bool) -> PredicateAssertion
resultShouldSatisfy init fn pred = pred `shouldHold` (evalState fn init)
