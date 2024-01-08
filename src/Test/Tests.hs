{-# LANGUAGE LambdaCase #-}

module Test.Tests where

import qualified App 
import qualified Args 
import Control.Monad
import Control.Monad.State (State, execState)
import Data.Maybe (listToMaybe)
import qualified Html 
import qualified Html.Parser  as HP
import qualified Parser  as P
import qualified Query  as Q
import qualified Query.Parser  as QP
import Result (Result (..), isError)
import System.Environment (getArgs)
import Test.SimpleTest
import qualified Test.SimpleTest.Expectation  as Exp
import qualified Test.SimpleTest.Mock  as Mock
import Test.SimpleTest.TestCase (TestCase, TestTree)
import Test.Util
import Text.Printf (printf)

tests =
  [ ("parser", parserTests),
    ("app", appLogicTests),
    ("io", ioTests)
  ]

main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> evalTestGroup False testSuite
    args' ->
      let tg' = do
            name <- listToMaybe $ dropWhile (`elem` ["-d", "--detailed"]) args'
            lookup name tests
          detailed = any (`elem` ["-d", "--detailed"]) args'
       in case tg' of
            Nothing -> evalTestGroup detailed testSuite
            Just tg -> evalTestGroup detailed tg

testSuite :: TestTree TestCase
testSuite =
  group
    "test suite"
    (map snd tests)

parserTests :: TestTree TestCase
parserTests =
  group
    "parser tests"
    [ group
        "html parser"
        [ group
            "text"
            ( let shouldParseAs = mkShouldParseAs HP.text
                  shouldParseAsWithRest = mkShouldParseAsWithRest HP.text
                  shouldFail = mkParseShouldFail HP.text
               in [ testCase "parses text without angle brackets 1" 10 ("some text" `shouldParseAs` "some text"),
                    testCase "parses text without angle brackets 2" 10 ("!@#$%^&&*()" `shouldParseAs` "!@#$%^&&*()"),
                    testCase "stops when the text contains angle brackets" 10 ("some text<asd" `shouldParseAsWithRest` ("some text", "<asd")),
                    testCase "fails when there are no characters before the angle brackets" 10 (shouldFail "<"),
                    testCase "fails on empty input" 10 (shouldFail "")
                  ]
            ),
          group
            "selfClosing"
            ( let shouldParseAs = mkShouldParseAs HP.selfClosing
                  shouldFail = mkParseShouldFail HP.selfClosing
               in [ testCase "parses a self closing tag without attributes" 20 ("<img/>" `shouldParseAs` ("img", [])),
                    testCase "parses a self closing tag with one attribute" 20 ("<img source=\"some_path\"/>" `shouldParseAs` ("img", [("source", Just "some_path")])),
                    testCase "parses a self closing tag with multiple attributes" 20 ("<img source=\"some_path\" width=\"100px\"/>" `shouldParseAs` ("img", [("source", Just "some_path"), ("width", Just "100px")])),
                    testCase "fails if tag is not self closing" 20 (shouldFail "<a>"),
                    testCase "fails if tag is invalid" 20 (shouldFail "<a")
                  ]
            ),
          group
            "openTag"
            ( let shouldParseAs = mkShouldParseAs HP.openTag
                  shouldFail = mkParseShouldFail HP.openTag
               in [ testCase "parses a tag without attributes" 20 ("<img>" `shouldParseAs` ("img", [])),
                    testCase "parses a tag with one attribute" 20 ("<a href=\"some_link\">" `shouldParseAs` ("a", [("href", Just "some_link")])),
                    testCase "parses a tag with multiple attributes" 20 ("<img source=\"some_path\" width=\"100px\">" `shouldParseAs` ("img", [("source", Just "some_path"), ("width", Just "100px")])),
                    testCase "fails if tag is self closing" 20 (shouldFail "<a/>"),
                    testCase "fails if tag is invalid" 20 (shouldFail "<a")
                  ]
            ),
          group
            "attributes"
            ( let shouldParseAs = mkShouldParseAs HP.attributes
                  shouldFail = mkParseShouldFail HP.attributes
               in [ testCase "parses one attribute" 25 ("a=\"b\"" `shouldParseAs` [("a", Just "b")]),
                    testCase "parses values between \"'\" (single quotes)" 25 ("a='b'" `shouldParseAs` [("a", Just "b")]),
                    testCase "parses multiple attributes" 25 ("a=\"b\" c=\"d\"" `shouldParseAs` [("a", Just "b"), ("c", Just "d")]),
                    testCase "parses one attribute without value" 25 ("a" `shouldParseAs` [("a", Nothing)]),
                    testCase "parses multiple attributes without value" 25 ("a b c" `shouldParseAs` fmap (\a -> (a,Nothing)) ["a", "b", "c"]),
                    testCase "parses mixed attributes" 25 ("a b=\"c\" x y z=\"q\"" `shouldParseAs` [("a", Nothing), ("b", Just "c"), ("x", Nothing), ("y", Nothing), ("z", Just "q")])
                  ]
            ),
          group
            "betweenHtmlTags"
            ( let shouldParseAs = mkShouldParseAs . HP.betweenHtmlTags
                  shouldFail = mkParseShouldFail . HP.betweenHtmlTags
               in [ testCase "parses value between tag without attributes" 20 (shouldParseAs (P.char 'b') "<a>b</a>" ('b', "a", [])),
                    testCase "parses value between tag with attributes" 20 (shouldParseAs (P.char 'b') "<a x=\"y\">b</a>" ('b', "a", [("x", Just "y")])),
                    testCase "fails when the inner parser fails" 20 (shouldFail (P.char 'c') "<a>b</a>"),
                    testCase "fails when the tag is self closing" 20 (shouldFail (P.char 'b') "<a/>"),
                    testCase "fails the closing tag is different" 20 (shouldFail (P.char 'b') "<a>b</c>")
                  ]
            ),
          group
            "htmlNode"
            ( let shouldParseAs = mkShouldParseAs HP.htmlNode
                  shouldFail = mkParseShouldFail HP.htmlNode
               in [ testCase "parses an empty node" 10 ("<a></a>" `shouldParseAs` (Html.HtmlNode "a" [] [])),
                    testCase
                      "parses a self closing tag"
                      10
                      ( "<br/>" `shouldParseAs` (Html.HtmlNode "br" [] [])
                      ),
                    testCase
                      "parses text within a node"
                      10
                      ("<p>hello</p>" `shouldParseAs` Html.HtmlNode "p" [] [Html.Text "hello"]),
                    testCase
                      "parses multiple nodes within a node"
                      10
                      ("<div><h1>title</h1><p>content</p></div>" `shouldParseAs` Html.HtmlNode "div" [] [Html.node "h1" [] [Html.Text "title"], Html.node "p" [] [Html.Text "content"]]),
                    testCase
                      "parses multiple nodes and text within a node"
                      10
                      ("<div>Some <b>bold</b> text</div>" `shouldParseAs` Html.HtmlNode "div" [] [Html.Text "Some ", Html.node "b" [] [Html.Text "bold"], Html.Text "text"])
                  ]
            ),
          group
            "html"
            ( let shouldParseAs = mkShouldParseAs HP.html
                  shouldFail = mkParseShouldFail HP.html
               in [ testCase "parses text" 10 ("some text" `shouldParseAs` Html.Text "some text"),
                    testCase "parses an empty node" 10 ("<a></a>" `shouldParseAs` (Html.node "a" [] [])),
                    testCase
                      "parses a self closing tag"
                      10
                      ( "<br/>" `shouldParseAs` (Html.node "br" [] [])
                      ),
                    testCase
                      "parses text within a node"
                      10
                      ("<p>hello</p>" `shouldParseAs` Html.node "p" [] [Html.Text "hello"]),
                    testCase
                      "parses multiple nodes and text within a node"
                      10
                      ("<div>Some <b>bold</b> text</div>" `shouldParseAs` Html.node "div" [] [Html.Text "Some ", Html.node "b" [] [Html.Text "bold"], Html.Text "text"])
                  ]
            )
        ]
    ]

appLogicTests :: TestTree TestCase
appLogicTests =
  group
    "app logic tests"
    [ group
        "parseArgs"
        [ testCase "fails if no arguments are provided" 25 (Args.parseArgs "prog.exe" [] `Exp.shouldBe` Error Args.NotEnoughArgs),
          testCase
            "assumes stdin if no files are provided"
            25
            ( Args.parseArgs "prog.exe" ["div"]
                `Exp.shouldBe` Success
                  Args.Args
                    { Args.argMaxResults = Nothing,
                      Args.argQuery = Q.fromTag "div",
                      Args.argFiles = Args.Stdin
                    }
            ),
          testCase
            "handles the case when files are provided"
            25
            ( Args.parseArgs "prog.exe" ["div", "file1.html", "file2.html"]
                `Exp.shouldBe` Success
                  Args.Args
                    { Args.argMaxResults = Nothing,
                      Args.argQuery = Q.fromTag "div",
                      Args.argFiles = Args.Files ["file1.html", "file2.html"]
                    }
            ),
          testCase
            "handles the case when --max-results is provided"
            25
            ( Args.parseArgs "prog.exe" ["div", "file1.html", "--max-results", "4", "file2.html"]
                `Exp.shouldBe` Success
                  Args.Args
                    { Args.argMaxResults = Just 4,
                      Args.argQuery = Q.fromTag "div",
                      Args.argFiles = Args.Files ["file1.html", "file2.html"]
                    }
            ),
          testCase
            "handles the case when --max-results is provided as first argument"
            25
            ( Args.parseArgs "prog.exe" ["--max-results", "4", "div", "file1.html", "file2.html"]
                `Exp.shouldBe` Success
                  Args.Args
                    { Args.argMaxResults = Just 4,
                      Args.argQuery = Q.fromTag "div",
                      Args.argFiles = Args.Files ["file1.html", "file2.html"]
                    }
            ),
          testCase
            "handles the case when -h is provided alone"
            10
            ( Exp.shouldHold (\case (Success (Args.Help _)) -> True; _ -> False) (Args.parseArgs "prog.exe" ["-h"])
            ),
          testCase
            "handles the case when --help is provided alone"
            10
            ( Exp.shouldHold (\case (Success (Args.Help _)) -> True; _ -> False) (Args.parseArgs "prog.exe" ["--help"])
            ),
          testCase
            "handles the case when -h is provided with other arguments"
            5
            ( Exp.shouldHold (\case (Success (Args.Help _)) -> True; _ -> False) (Args.parseArgs "prog.exe" ["div", "file1.html", "file2.html", "-h"])
            )
        ],
      group
        "parseContents"
        [ testCase
            "parses the contents of one file"
            50
            (App.parseContents [("test.html", "<div></div>")] `Exp.shouldBe` Success [("test.html", Html.Document [Html.node "div" [] []])]),
          testCase
            "parses the contents of multiple files"
            50
            ( App.parseContents
                [("test1.html", "<p></p>"), ("test2.html", "<div><p>Text</p></div>")]
                `Exp.shouldBe` Success
                  [ ("test1.html", Html.Document [Html.node "p" [] []]),
                    ("test2.html", Html.Document [Html.node "div" [] [Html.node "p" [] [Html.Text "Text"]]])
                  ]
            )
        ],
      group
        "searchFiles"
        [ testCase
            "handles one result in one file"
            50
            (App.searchFiles (Q.fromTag "div") [("file1.html", (Html.Document [Html.node "div" [] []]))] `Exp.shouldBe` [App.FileMatch "file1.html" (Html.node "div" [] [])]),
          testCase
            "handles multiple results in one file"
            50
            ( App.searchFiles
                (Q.fromTag "div")
                [ ( "file1.html",
                    Html.Document
                      [ Html.node "div" [("class", Just "first")] [],
                        Html.node "div" [("class", Just "second")] []
                      ]
                  )
                ]
                `Exp.shouldBe` [ App.FileMatch "file1.html" (Html.node "div" [("class", Just "first")] []),
                                 App.FileMatch "file1.html" (Html.node "div" [("class", Just "second")] [])
                               ]
            )
        ]
    ]

ioTests :: TestTree TestCase
ioTests =
  group
    "io tests"
    [ group
        "readContents"
        [ group
            "with Args.Stdin"
            ( let fn = App.readContents Args.Stdin
                  state = testFs
               in [ testCase "reads stdin" 50 (shouldReadStdin state fn),
                    testCase "does not read other files" 50 (Exp.and (shouldNotReadFile "file1.html" state fn) (shouldNotReadFile "file2.html" state fn))
                  ]
            ),
          group
            "with Args.Files"
            ( let fn = App.readContents $ Args.Files ["file1.html", "file2.html"]
                  state = testFs
                  state' = testFs `Mock.addFile` ("file3.html", "<p></p>")
               in [ testCase "reads both files" 50 (Exp.and (shouldReadFromFile "file1.html" state fn) (shouldReadFromFile "file2.html" state fn)),
                    testCase "does not touch other files" 50 (shouldNotTouchFile "file3.html" state' fn),
                    testCase "fails if a file is not found" 50 (shouldHaveResult emptyFs fn (Error $ App.FileNotFound "file1.html"))
                  ]
            )
        ],
      group
        "maybeReadFile"
        [ testCase "reads from file when it exists" 50 (shouldReadFromFile "file1.html" testFs (App.maybeReadFile "file1.html")),
          testCase "checks if the file exists" 50 (shouldCheckIfFileExists "file1.html" testFs (App.maybeReadFile "file1.html")),
          testCase "does not read from unrelated files" 50 (shouldNotReadFile "file2.html" testFs (App.maybeReadFile "file1.html"))
        ],
      group
        "printMatches"
        ( let node1 = Html.node "div" [] []
              node2 = Html.node "p" [] [Html.Text "text"]
           in [ testCase "write to stdout the name of matched files" 50 (shouldWriteAllToStdout ["file1.html"] testFs (App.printMatches [App.FileMatch "file1.html" node1])),
                testCase
                  "write to stdout the name of multiple matched files"
                  50
                  ( shouldWriteAllToStdout
                      ["file1.html", "file2.html"]
                      testFs
                      ( App.printMatches [App.FileMatch "file1.html" node1, App.FileMatch "file2.html" node2]
                      )
                  ),
                testCase
                  "write to stdout the contents of matched files 1"
                  25
                  ( shouldWriteAllToStdout
                      ["file1.html", show node1]
                      testFs
                      ( App.printMatches [App.FileMatch "file1.html" node1]
                      )
                  ),
                testCase
                  "write to stdout the contents of matched files 2"
                  25
                  ( shouldWriteAllToStdout
                      ["file2.html", show node2]
                      testFs
                      ( App.printMatches [App.FileMatch "file2.html" node2]
                      )
                  )
              ]
        )
    ]

emptyFs =
  Mock.makeMockIOState
    []

testFs =
  Mock.makeMockIOState
    [ ("file1.html", "<div></div>"),
      ("file2.html", "<p></p>")
    ]
