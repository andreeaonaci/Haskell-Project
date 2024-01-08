module Query where

data QuerySelector = QuerySelector {selectorTag :: Maybe String, selectorIds :: [String], selectorClasses :: [String], selectorAttributes :: [(String, Maybe String)]} deriving (Eq, Show)

data Query
  = Selector QuerySelector
  | -- | @A B@ Select descendants of A that match B
    Descendant Query Query
  | -- | @A > B@ Select *direct* children of A that match B
    Child Query Query
  | -- | @A, B@ Select nodes that match A *or* B
    List Query Query
  deriving (Eq, Show)

allAttrs :: QuerySelector -> [(String, Maybe String)]
allAttrs QuerySelector {selectorTag = _, selectorIds = ids, selectorClasses = classes, selectorAttributes = attrs} = attrs ++ ((\a -> ("id",a)) . Just <$> ids) ++ ((\a -> ("class",a)) . Just <$> classes)

fromTag :: String -> Query
fromTag tag = Selector $ QuerySelector (Just tag) [] [] []
