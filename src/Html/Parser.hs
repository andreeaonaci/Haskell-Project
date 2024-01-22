module Html.Parser where

import Html
import qualified Parser  as P
import Result

-- | Parses HTML text, consuming input as long as it doesn't contain the characters < or >
--
-- >>> P.parse text "some text"
-- Success "some text"
--
-- >>> P.parse text "!@#$%^&&*()"
-- Success "!@#$%^&&*()"
--
-- >>> P.runParser text "some text<"
-- Success ("some text","<")
--
-- >>> P.runParser text "<some text>"
-- Error (UnexpectedInput {gotInput = "<some text>", expectedInput = "At least one character"})
text :: P.Parser String
text = P.pWith (P.many (P.satisfies (\c -> c /= '<') "Not '<'")) $ \chars ->
    let checkNonEmpty [] = P.fail "At least one character"
        checkNonEmpty xs = P.succeed xs
    in checkNonEmpty chars

-- | Parses a self closing tag
--
-- A self closing tag is an identifier followed by a possibly empty list of attributes (see @attributes@) between < and />
--
-- Some useful functions:
-- - @Parser.ident@
-- - @Parser.between@
-- - @Parser.tag@
-- - @Parser.char@
--
-- >>> P.parse selfClosing "<a/>"
-- Success ("a",[])
--
-- >>> P.parse selfClosing "<a>"
-- Error (UnexpectedInput {gotInput = ">", expectedInput = "character '/'"})
--
-- >>> P.parse selfClosing "<a x=\"y\"/>"
-- Success ("a",[("x",Just "y")])
--
-- >>> P.parse selfClosing "<a x/>"
-- Success ("a",[("x",Nothing)])

selfClosing :: P.Parser (String, [(String, Maybe String)])
selfClosing = P.between (P.char '<') (P.char '/' `P.pThen` P.char '>' `P.pThen` P.succeed []) tagContent
  where
    tagContent = P.pWith P.ident $ \ident ->
      let attributes = P.many (P.ws1 `P.pThen` attribute)
      in P.pMap (\attrs -> (ident, attrs)) attributes

    attribute :: P.Parser (String, Maybe String)
    attribute = P.pWith P.ident $ \name ->
      let attributeValue = P.opt (P.char '=' `P.pThen` attributeValue')
      in P.pMap (\valueOpt -> (name, valueOpt)) attributeValue

    attributeValue' :: P.Parser String
    attributeValue' = P.between (P.char '"') (P.char '"') (P.many (P.satisfies (/= '"') "character")) `P.orElse`
                     P.between (P.char '\'') (P.char '\'') (P.many (P.satisfies (/= '\'') "character")) `P.orElse`
                     P.many (P.satisfies (\c -> c /= ' ' && c /= '>') "character")

-- | Parses an opening tag
--
-- A tag is an identifier followed by a possibly empty list of attributes (see @attributes@) between < and >
--
-- Note: There might be whitespace between the identifier and >
--
-- Some useful functions:
-- - @Parser.between@
-- - @Parser.tag@
-- - @Parser.char@
--
-- >>> P.parse openTag "<a>"
-- Success ("a",[])
--
-- >>> P.parse openTag "<a >"
-- Success ("a",[])

openTag :: P.Parser (String, [(String, Maybe String)])
openTag = P.between (P.char '<') (P.char '>') tagContent
  where
    tagContent = P.pWith P.ident $ \ident ->
      P.pMap (\attrs -> (ident, attrs)) attributes

    attributes :: P.Parser [(String, Maybe String)]
    attributes = P.many (P.ws1 `P.pThen` attribute)

    attribute :: P.Parser (String, Maybe String)
    attribute = P.pWith P.ident $ \name ->
      P.pMap (\valueOpt -> (name, valueOpt)) (P.opt (P.char '=' `P.pThen` attributeValue))

    attributeValue :: P.Parser String
    attributeValue = P.between (P.char '"') (P.char '"') (P.many (P.satisfies (/= '"') "character")) `P.orElse`
                    P.between (P.char '\'') (P.char '\'') (P.many (P.satisfies (/= '\'') "character")) `P.orElse`
                    P.many (P.satisfies (\c -> c /= ' ' && c /= '>') "character")

-- | Parses a possibly empty list of attributes
--
-- An attribute is a key, optionally followed by = and a value between double quotes or single qoutes.
-- The attributes are separated by whitespace.
--
-- Some useful functions:
-- - @Parser.sepBy@
-- - @Parser.between@
-- - @Parser.opt@
-- - @Parser.orElse@
--
-- >>> P.parse attributes "a=\"b\""
-- Success [("a",Just "b")]
--
-- >>> P.parse attributes "a='b'"
-- Success [("a",Just "b")]
--
-- >>> P.parse attributes "a=\"\""
-- Success [("a",Just "")]
--
-- >>> P.parse attributes "a b=\"x\""
-- Success [("a",Nothing),("b",Just "x")]
--
-- >>> P.parse attributes "a b"
-- Success [("a",Nothing),("b",Nothing)]
-- Parse a key
-- Parse a key
attributes :: P.Parser [(String, Maybe String)]
attributes = P.sepBySome P.ws1 attribute

attribute :: P.Parser (String, Maybe String)
attribute = attributeWithValue `P.orElse` attributeWithoutValue

attributeWithValue :: P.Parser (String, Maybe String)
attributeWithValue = P.pWith P.ident $ \name ->
  let parseValue = P.char '=' `P.pThen` attributeValue
  in P.pMap (\value -> (name, Just value)) parseValue

attributeWithoutValue :: P.Parser (String, Maybe String)
attributeWithoutValue = P.pMap (\name -> (name, Nothing)) P.ident

attributeValue :: P.Parser String
attributeValue = P.between (P.char '"') (P.char '"') P.ident `P.orElse` P.between (P.char '\'') (P.char '\'') P.ident

-- | Parses the given closing tag
--
-- A closing tag is an identifier between </ and >
--
-- Note: There might be whitespace between the identifier and >
--
-- Some useful functions:
-- - @Parser.tag@
--
-- >>> P.parse (closingTag "a") "</a>"
-- Success ()
--
-- >>> P.parse (closingTag "a") "</a  >"
-- Success ()
--
-- >>> P.parse (closingTag "a") "</div>"
-- Error (UnexpectedInput {gotInput = "d", expectedInput = "character 'a'"})

closingTag :: String -> P.Parser ()
closingTag tag =
  P.tag "</" `P.pThen` P.ws `P.pThen` tagParser `P.pThen` P.ws `P.pThen` P.tag ">" `P.pThen` P.succeed ()
  where
    tagParser = P.tag tag

-- | Run a parser between HTML tags, checking that the opening and closing tag match.
--
-- Some useful functions:
-- - @openTag@
-- - @closingTag@
-- - @Parser.pWith@
-- 
-- >>> P.parse (betweenHtmlTags (P.char 'x')) "<a>x</a>"
-- Success ('x',"a",[])
--
-- >>> P.parse (betweenHtmlTags (P.char 'x')) "<a>x</b>"
-- Error (UnexpectedInput {gotInput = "b", expectedInput = "character 'a'"})
--
-- >>> P.parse (betweenHtmlTags (P.char 'x')) "<a y=\"z\">x</a>"
-- Success ('x',"a",[("y",Just "z")])
--
-- >>> P.parse (betweenHtmlTags (P.char 'x')) "<a y>x</a>"
-- Success ('x',"a",[("y",Nothing)])

betweenHtmlTags :: P.Parser a -> P.Parser (a, String, [(String, Maybe String)])
betweenHtmlTags p = P.pWith openTag (\(tag, attrs) -> P.pWith p (\result -> P.pWith (closingTag tag) (\() -> P.succeed (result, tag, attrs))))

-- | Parses a HTML node
--
-- A HTML node is one of:
-- - a self closing tag
-- - an opening and closing tag, with more HTML between them
--
-- Some useful functions:
-- - @Parser.pMap@
-- - @Parser.orElse@
-- - @html@
-- - @betweenHtmlTags@
-- - @selfClosing@
--
-- >>> P.parse htmlNode "<a/>"
-- Success (HtmlNode {nodeTag = "a", nodeAttrs = [], nodeChildren = []})
--
-- >>> P.parse htmlNode "<a>b</a>"
-- Success (HtmlNode {nodeTag = "a", nodeAttrs = [], nodeChildren = [b]})
--
-- >>> P.parse htmlNode "<a><b>text</b></a>"
-- Success (HtmlNode {nodeTag = "a", nodeAttrs = [], nodeChildren = [<b>
--   text
-- </b>]})

-- htmlNode :: P.Parser HtmlNode
-- htmlNode =
--   P.orElse selfClosingTag htmlTag
--   where
--     selfClosingTag = P.pMap (\(tagName, attrs) -> HtmlNode tagName attrs []) selfClosing

--     htmlTag = 
--       P.pMap (\(content, tagName, attrs) -> HtmlNode tagName attrs (map trimWhitespace content)) 
--       (betweenHtmlTags (P.many html))

--     trimWhitespace (Text str) = Text (trimLeading str)
--     trimWhitespace node@(Node _) = node

--     trimLeading :: String -> String
--     trimLeading = dropWhile (\c -> c == ' ')

--     trim :: String -> String
--     trim = f . f
--       where f = reverse . dropWhile (\c -> c == ' ')

htmlNode :: P.Parser HtmlNode
htmlNode = P.orElse selfClosingTag htmlTag
  where
    selfClosingTag = let parseSelfClosing = P.pMap (\(tagName, attrs) -> HtmlNode tagName attrs []) selfClosing
                    in parseSelfClosing

    htmlTag = let parseHtmlTag = P.pMap (\(content, tagName, attrs) -> HtmlNode tagName attrs content)
                    (betweenHtmlTags (P.many html))
              in parseHtmlTag

-- | Parses a HTML node or a text node
--
-- Some useful functions:
-- - @Parser.pMap@
-- - @Parser.orElse@
-- - @htmlNode@
-- - @text@
--
-- >>> P.parse html "<a y=\"z\"><b>x</b></a>"
-- Success <a y="z">
--   <b>
--     x
--   </b>
-- </a>
--
-- >>> P.parse html "<a y=\"z\"><b>x <z></z></b></a>"
-- Success <a y="z">
--   <b>
--     x
--     <z/>
--   </b>
-- </a>

html :: P.Parser Html
html = P.orElse (P.pMap Node htmlNode) (P.pMap Text text)
  where
    text = P.some (P.satisfies (\c -> c /= '<') "text")

document :: P.Parser Document
document = P.eof $ P.pMap Document $ P.some html

parse :: String -> Result P.ParseError Document
parse = P.parse document

-- >>> parseNode "<div></div>"
-- Success (HtmlNode {nodeTag = "div", nodeAttrs = [], nodeChildren = []})
--
-- >>> parseNode "<div></div><p></p>"
-- Error (ExpectedEOF {remainingInput = "<p></p>"})
parseNode :: String -> Result P.ParseError HtmlNode
parseNode = P.parse (P.eof htmlNode)
