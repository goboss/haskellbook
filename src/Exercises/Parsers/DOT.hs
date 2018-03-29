module Exercises.Parsers.DOT where

import Data.Monoid ((<>))
import Control.Applicative ((<|>))
import Text.Trifecta

-- Exercise
-- Write a parser for the DOT language 14 that Graphviz uses to
-- express graphs in plain-text (http://www.graphviz.org/doc/info/lang.html).

data GraphType = Graph | Digraph deriving Show
type GraphId = String
type Attribute = (GraphId, GraphId)
data Node = Node GraphId (Maybe Port) [Attribute] deriving Show
data Compass = N | NE | E | SE | S | SW | W | NW | C | None deriving Show
type Port = Either Compass (GraphId, (Maybe Compass))

graphType :: Parser GraphType
graphType =
  fmap (const Graph) (string "graph")
  <|>
  fmap (const Digraph) (string "digraph")

graphId :: Parser GraphId
graphId = stringId <|> numeralId <|> quotedId <|> htmlId
  where stringId :: Parser GraphId
        stringId =
          let beginning = char '_' <|> letter
              middle    = beginning <|> digit
          in some beginning <> many middle
        numeralId :: Parser GraphId
        numeralId =
          let e2str (Left i)  = show i
              e2str (Right d) = show d
          in fmap e2str integerOrDouble
        quotedId :: Parser GraphId
        quotedId =
          let escapedChar = (char '\\') *> (oneOf ['\\', '"'])
              normalChar = noneOf "\""
          in between (char '"') (char '"') (many (escapedChar <|> normalChar))
        htmlId :: Parser GraphId
        htmlId =
          between (char '<') (char '>') (many (noneOf "<>"))

attr :: Parser Attribute
attr = do
  key <- token graphId
  _   <- token (char '=')
  val <- token graphId
  return (key, val)

attrList :: Parser [Attribute]
attrList = open *> sepBy attr sep <* close
  where open  = token (char '[')
        close = token (char ']')
        sep = token (char ',' <|> char ';')

compass :: Parser Compass
compass = choice
  [
    string "ne" *> (return NE),
    string "n"  *> (return N),
    string "se" *> (return SE),
    string "e"  *> (return E),
    string "sw" *> (return SW),
    string "s"  *> (return S),
    string "nw" *> (return NW),
    string "w"  *> (return W),
    string "c"  *> (return C),
    string "_"  *> (return None)
  ]

port :: Parser Port
port = fmap Left (try left) <|> fmap Right right
  where left = sep *> compass
        right = do
          _ <- sep
          i <- token graphId
          c <- optional left
          return (i, c)
        sep = token (char ':')

node :: Parser Node
node = do
  gi <- token graphId
  po <- optional (token port)
  as <- optional attrList
  case as of
    Just a  -> return (Node gi po a)
    Nothing -> return (Node gi po [])
