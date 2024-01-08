module Query.Parser where

import qualified Parser  as P
import Query (Query (..), QuerySelector (..))
import Result

data AttrSelector = Id String | Class String | Attr (String, Maybe String)

tag :: P.Parser String
tag = P.ident

class' :: P.Parser String
class' = P.char '.' `P.pThen` P.ident

id' :: P.Parser String
id' = P.char '#' `P.pThen` P.ident

attribute :: P.Parser (String, Maybe String)
attribute = P.char '$' `P.pThen` (P.ident `P.andThen` P.opt ((P.char ':') `P.pThen` P.ident))

descendant :: P.Parser Query
descendant = P.pMap (\(q1, _, q2) -> Descendant q1 q2) $ P.andThen3 selector P.ws1 query

child :: P.Parser Query
child = P.pMap (\(q1, _, q2) -> Child q1 q2) $ P.andThen3 selector (P.between P.ws P.ws (P.char '>')) query

list :: P.Parser Query
list = P.pMap (\(q1, _, q2) -> List q1 q2) $ P.andThen3 selector (P.between P.ws P.ws (P.char ',')) query

selectorAttr :: P.Parser AttrSelector
selectorAttr = P.pMap Class class' `P.orElse` P.pMap Id id' `P.orElse` P.pMap Attr attribute

selector :: P.Parser Query
selector = P.pMap Selector selector'

selector' :: P.Parser QuerySelector
selector' = P.pMap2 f (P.opt tag) (P.many selectorAttr)
  where
    f tag attrs = QuerySelector {selectorTag = tag, selectorIds = [x | Id x <- attrs], selectorClasses = [x | Class x <- attrs], selectorAttributes = [attr | Attr attr <- attrs]}

-- >>> P.parse query "h1"
-- Success (Selector (QuerySelector {selectorTag = Just "h1", selectorIds = [], selectorClasses = [], selectorAttributes = []}))
--
-- >>> P.parse query "#page-title"
-- Success (Selector (QuerySelector {selectorTag = Nothing, selectorIds = ["page-title"], selectorClasses = [], selectorAttributes = []}))
--
-- >>> P.parse query "#page-title#other.rounded"
-- Success (Selector (QuerySelector {selectorTag = Nothing, selectorIds = ["page-title","other"], selectorClasses = ["rounded"], selectorAttributes = []}))
--
-- >>> P.parse query ".rounded"
-- Success (Selector (QuerySelector {selectorTag = Nothing, selectorIds = [], selectorClasses = ["rounded"], selectorAttributes = []}))
--
-- >>> P.parse query ".rounded.p1"
-- Success (Selector (QuerySelector {selectorTag = Nothing, selectorIds = [], selectorClasses = ["rounded","p1"], selectorAttributes = []}))
--
-- >>> P.parse query "a$x"
-- Success (Selector (QuerySelector {selectorTag = Just "a", selectorIds = [], selectorClasses = [], selectorAttributes = [("x",Nothing)]}))
--
-- >>> P.parse query "a$x:y"
-- Success (Selector (QuerySelector {selectorTag = Just "a", selectorIds = [], selectorClasses = [], selectorAttributes = [("x",Just "y")]}))
--
-- >>> P.parse query "a$x$y"
-- Success (Selector (QuerySelector {selectorTag = Just "a", selectorIds = [], selectorClasses = [], selectorAttributes = [("x",Nothing),("y",Nothing)]}))
--
-- >>> P.parse query "a$x:y$z"
-- Success (Selector (QuerySelector {selectorTag = Just "a", selectorIds = [], selectorClasses = [], selectorAttributes = [("x",Just "y"),("z",Nothing)]}))
--
-- >>> P.parse query ".rounded .p-1"
-- Success (Descendant (Selector (QuerySelector {selectorTag = Nothing, selectorIds = [], selectorClasses = ["rounded"], selectorAttributes = []})) (Selector (QuerySelector {selectorTag = Nothing, selectorIds = [], selectorClasses = ["p-1"], selectorAttributes = []})))
--
-- >>> P.parse query "ul > li"
-- Success (Child (Selector (QuerySelector {selectorTag = Just "ul", selectorIds = [], selectorClasses = [], selectorAttributes = []})) (Selector (QuerySelector {selectorTag = Just "li", selectorIds = [], selectorClasses = [], selectorAttributes = []})))
--
-- >>> P.parse query "ul > li > p"
-- Success (Child (Selector (QuerySelector {selectorTag = Just "ul", selectorIds = [], selectorClasses = [], selectorAttributes = []})) (Child (Selector (QuerySelector {selectorTag = Just "li", selectorIds = [], selectorClasses = [], selectorAttributes = []})) (Selector (QuerySelector {selectorTag = Just "p", selectorIds = [], selectorClasses = [], selectorAttributes = []}))))
--
-- >>> P.parse query "ul,li"
-- Success (List (Selector (QuerySelector {selectorTag = Just "ul", selectorIds = [], selectorClasses = [], selectorAttributes = []})) (Selector (QuerySelector {selectorTag = Just "li", selectorIds = [], selectorClasses = [], selectorAttributes = []})))
--
query :: P.Parser Query
query = child `P.orElse` list `P.orElse` descendant `P.orElse` selector

parse :: String -> Result P.ParseError Query
parse input = P.parse query input

parseSelector :: String -> Result P.ParseError QuerySelector
parseSelector input = P.parse selector' input