module Args where

import Data.List
import qualified Query  as Q
import qualified Query.Parser  as Q
import Result
import Text.Read (readMaybe)

data SearchedFiles
  = Stdin
  | Files [String]
  deriving (Eq, Show)

data Args
  = Args
      { -- | Given as the first argument
        argQuery :: Q.Query,
        -- | Rest of arguments
        argFiles :: SearchedFiles,
        -- | Given as @--max-results value@
        argMaxResults :: Maybe Int
      }
  | Help String
  deriving (Eq, Show)

data ParseArgsError
  = NotEnoughArgs
  | InvalidArgs String
  deriving (Eq, Show)

usage progName = "Usage: " ++ progName ++ " query pattern [files...]"

-- | Parses the program arguments
--
-- First parameter is the name of the executable. The second parameter (of type @[String])@) contains the list of arguments.
--
-- The first positional argument represents the query and the rest of the arguments represent the files to check.
-- If no files are provided (i.e. only one argument is given, which represents the query), stdin should be used (i.e. the @Stdin@ constructor for @SearchedFiles@).
-- The @--max-results value@ flag is optional, and might be given in any position.
-- If the -h or --help arguments are provided, the function should return @Success@ and the usage in the @Help@ constructor
--
-- Useful functions:
-- - @separateFlags@
-- - @Query.Parser.parse@
--
-- >>> parseArgs "html-search.exe" ["-h"]
-- Success (Help "Usage: html-search.exe query pattern [files...]")
--
-- >>> parseArgs "html-search.exe" ["div > h1.title", "file.html"]
-- Success (Args {argQuery = Child (Selector (QuerySelector {selectorTag = Just "div", selectorIds = [], selectorClasses = [], selectorAttributes = []})) (Selector (QuerySelector {selectorTag = Just "h1", selectorIds = [], selectorClasses = ["title"], selectorAttributes = []})), argFiles = Files ["file.html"], argMaxResults = Nothing})
--
-- >>> parseArgs "html-search.exe" ["div > h1"]
-- Success (Args {argQuery = Child (Selector (QuerySelector {selectorTag = Just "div", selectorIds = [], selectorClasses = [], selectorAttributes = []})) (Selector (QuerySelector {selectorTag = Just "h1", selectorIds = [], selectorClasses = [], selectorAttributes = []})), argFiles = Stdin, argMaxResults = Nothing})
--
-- >>> parseArgs "html-search.exe" ["div h1", "file1.html", "file2.html"]
-- Success (Args {argQuery = Descendant (Selector (QuerySelector {selectorTag = Just "div", selectorIds = [], selectorClasses = [], selectorAttributes = []})) (Selector (QuerySelector {selectorTag = Just "h1", selectorIds = [], selectorClasses = [], selectorAttributes = []})), argFiles = Files ["file1.html","file2.html"], argMaxResults = Nothing})
--
-- >>> parseArgs "html-search.exe" ["div > h1", "file.html", "--max-results", "1"]
-- Success (Args {argQuery = Child (Selector (QuerySelector {selectorTag = Just "div", selectorIds = [], selectorClasses = [], selectorAttributes = []})) (Selector (QuerySelector {selectorTag = Just "h1", selectorIds = [], selectorClasses = [], selectorAttributes = []})), argFiles = Files ["file.html"], argMaxResults = Just 1})
--
-- >>> parseArgs "html-search.exe" ["div > h1", "--max-results", "1", "file.html"]
-- Success (Args {argQuery = Child (Selector (QuerySelector {selectorTag = Just "div", selectorIds = [], selectorClasses = [], selectorAttributes = []})) (Selector (QuerySelector {selectorTag = Just "h1", selectorIds = [], selectorClasses = [], selectorAttributes = []})), argFiles = Files ["file.html"], argMaxResults = Just 1})

--without haskell style
-- parseArgs :: String -> [String] -> Result ParseArgsError Args
-- parseArgs progName args
--   | null args = Error NotEnoughArgs
--   | any (`elem` ["-h", "--help"]) args = Success $ Help (usage progName)
--   | otherwise = case separateFlags args of
--       Success (flags, query:files) -> case Q.parse query of
--         Success q -> case lookup "--max-results" flags of
--           Just maxResultsStr -> case readMaybe maxResultsStr of
--             Just maxResults -> Success $ Args q (if null files then Stdin else Files files) (Just maxResults)
--             Nothing -> Error $ InvalidArgs "Invalid max results value"
--           Nothing -> Success $ Args q (if null files then Stdin else Files files) Nothing
--         Error err -> Error $ InvalidArgs (show err)
--       Error err -> Error err

parseArgs :: String -> [String] -> Result ParseArgsError Args
parseArgs progName args
  | "-h" `elem` args || "--help" `elem` args = Success $ Help (usage progName)
  | null args = Error NotEnoughArgs
  | otherwise = handleArgs (separateFlags args)
  where
    handleArgs :: Result ParseArgsError ([(String, String)], [String]) -> Result ParseArgsError Args
    handleArgs (Error err) = Error err
    handleArgs (Success (flags, query : files)) =
      case Q.parse query of
        Error err -> Error $ InvalidArgs (show err)
        Success q ->
          let maxResults = readMaxResults flags
              fileSource = if null files then Stdin else Files files
          in Success $ Args q fileSource maxResults

    readMaxResults :: [(String, String)] -> Maybe Int
    readMaxResults flags = do
      maxResultsStr <- lookup "--max-results" flags
      readMaybe maxResultsStr

-- | Separates positional arguments and flags
--
-- >>> separateFlags ["--flag1", "value1", "--flag2", "value2", "positional1"]
--
-- >>> separateFlags ["positional1", "--flag1", "value1", "--flag2", "value2"]
separateFlags ::
  [String] -> Result ParseArgsError ([(String, String)], [String])
separateFlags [] = Success ([], [])
separateFlags [last] =
  if "--" `isPrefixOf` last
    then Error $ InvalidArgs "Flag without value"
    else Success ([], [last])
separateFlags (arg : value : rest)
  | "--" `isPrefixOf` arg = do
      (flags, positionals) <- separateFlags rest
      return ((arg, value) : flags, positionals)
  | otherwise = do
      (flags, positionals) <- separateFlags (value : rest)
      return (flags, arg : positionals)
