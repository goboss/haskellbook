module Exercises.Parsers.DOT where

import Data.Monoid ((<>))
import Control.Applicative ((<|>), liftA2)
import Text.Trifecta

-- Exercise
-- Write a parser for the DOT language that Graphviz uses to
-- express graphs in plain-text (http://www.graphviz.org/doc/info/lang.html).

data GraphType = Graph | Digraph | Subgraph deriving Show
type GraphId = String

type Attribute = (GraphId, GraphId)

data Compass = N | NE | E | SE | S | SW | W | NW | C | None deriving Show
type Port = Either Compass (GraphId, (Maybe Compass))

type NodeId = (GraphId, Maybe Port)

data Node = Node NodeId [Attribute] deriving Show

data EdgeRHS = Directed NodeId | Undirected NodeId deriving Show
data Edge = Edge NodeId [EdgeRHS] [Attribute] deriving Show

type Assign = (GraphId, GraphId)

graphType :: Parser GraphType
graphType =
  fmap (const Graph) (string "graph")
  <|>
  fmap (const Digraph) (string "digraph")
  <|>
  fmap (const Subgraph) (string "subgraph")

graphId :: Parser GraphId
graphId = token (stringId <|> numeralId <|> quotedId <|> htmlId)
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

nodeId :: Parser NodeId
nodeId = token (liftA2 (,) graphId (optional port))

node :: Parser Node
node = do
  ni <- nodeId
  as <- optional attrList
  case as of
    Just a  -> return (Node ni a)
    Nothing -> return (Node ni [])

edgeRHS :: Parser EdgeRHS
edgeRHS =
  fmap Directed (token (string "->") *> nodeId)
  <|>
  fmap Undirected (token (string "--") *> nodeId)

edge :: Parser Edge
edge = do
  eid <- nodeId
  rhs <- many edgeRHS
  att <- optional attrList
  case att of
    Just a  -> return (Edge eid rhs a)
    Nothing -> return (Edge eid rhs [])

assign :: Parser Assign
assign = token (liftA2 (,) graphId (token (char '=') *> graphId))

strict :: Parser ()
strict = token (optional (string "strict")) *> return ()

comment :: Parser String
comment =
  let
    end :: Parser Char
    end = char '\n' <|> (eof >> return '\n')
    single :: Parser String
    single = (string "//" <|> string "#") *> manyTill anyChar (try end)
    multi :: Parser String
    multi = string "/*" *> manyTill anyChar (try (string "*/"))
  in
    single <|> multi
