module Html where

import qualified Data.List  as L

data HtmlNode = HtmlNode {nodeTag :: String, nodeAttrs :: [(String, Maybe String)], nodeChildren :: [Html]} deriving (Show, Eq)

data Html
  = Text String
  | Node HtmlNode
  deriving (Eq)

node :: String -> [(String, Maybe String)] -> [Html] -> Html
node tag attrs children = Node $ HtmlNode tag attrs children

data Document = Document {docNodes :: [Html]} deriving (Eq)

instance Show Html where
  show html = showIndent "" html
    where
      showIndent :: String -> Html -> String
      showIndent indent (Text text) = indent ++ text
      showIndent indent (Node (HtmlNode {nodeTag = tag, nodeAttrs = attrs, nodeChildren = children})) =
        if null children
          then indent ++ openTag ++ "/>"
          else indent ++ openTag ++ ">" ++ "\n" ++ (L.intercalate "\n" $ (showIndent ("  " ++ indent)) <$> children) ++ "\n" ++ indent ++ "</" ++ tag ++ ">"
        where
          openTag = "<" ++ tag ++ if null attrs then "" else " " ++ L.intercalate " " ((fmap $ uncurry showAttr) attrs)
          showAttr name Nothing = name
          showAttr name (Just value) = name ++ "=" ++ "\"" ++ value ++ "\""

instance Show Document where
  show (Document nodes) = L.intercalate "\n" (show <$> nodes)
