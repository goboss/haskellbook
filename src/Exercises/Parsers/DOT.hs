{-# LANGUAGE QuasiQuotes #-}

module Exercises.Parsers.DOT where

import Data.Monoid ((<>))
import Control.Applicative ((<|>), liftA2)
import Text.Trifecta
import Text.RawString.QQ

-- Exercise
-- Write a parser for the DOT language that Graphviz uses to
-- express graphs in plain-text (http://www.graphviz.org/doc/info/lang.html).

type ID = String

graphId :: Parser ID
graphId = token (stringId <|> numeralId <|> quotedId <|> htmlId)
  where stringId :: Parser ID
        stringId =
          let beginning = char '_' <|> letter
              middle    = beginning <|> digit
          in some beginning <> many middle
        numeralId :: Parser ID
        numeralId =
          let e2str (Left i)  = show i
              e2str (Right d) = show d
          in fmap e2str integerOrDouble
        quotedId :: Parser ID
        quotedId =
          let escapedChar = (char '\\') *> (oneOf ['\\', '"'])
              normalChar = noneOf "\""
          in between (char '"') (char '"') (many (escapedChar <|> normalChar))
        htmlId :: Parser ID
        htmlId =
          between (char '<') (char '>') (many (noneOf "<>"))

type Attr = (ID, ID)
data Attribute =
    GraphAttribute [Attr]
  | NodeAttribute [Attr]
  | EdgeAttribute [Attr]
  deriving Show

attr :: Parser Attr
attr = token (liftA2 (,) graphId (token (char '=') *> graphId))

attrList :: Parser [Attr]
attrList = fmap concat (many lst)
  where lst = open *> sepBy attr sep <* close
        open  = token (char '[')
        close = token (char ']')
        sep = token (char ',' <|> char ';')

attribute :: Parser Attribute
attribute = graphAttr <|> nodeAttr <|> edgeAttr
  where graphAttr :: Parser Attribute
        graphAttr = mkAttr "graph" GraphAttribute
        nodeAttr :: Parser Attribute
        nodeAttr = mkAttr "node" NodeAttribute
        edgeAttr :: Parser Attribute
        edgeAttr = mkAttr "edge" EdgeAttribute
        mkAttr :: String -> ([Attr] -> Attribute) -> Parser Attribute
        mkAttr name cons = fmap cons (token (string name) *> attrList)

data Compass = N | NE | E | SE | S | SW | W | NW | C | None deriving Show
type Port = Either Compass (ID, (Maybe Compass))

type NodeId = (ID, Maybe Port)
data Node = Node NodeId [Attr] deriving Show

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

data EdgeRHS = Directed NodeId | Undirected NodeId deriving Show
data Edge = Edge NodeId [EdgeRHS] [Attr] deriving Show

edgeRHS :: Parser EdgeRHS
edgeRHS =
  fmap Directed (token (string "->") *> nodeId)
  <|>
  fmap Undirected (token (string "--") *> nodeId)

edge :: Parser Edge
edge = do
  eid <- nodeId
  rhs <- some edgeRHS
  att <- optional attrList
  case att of
    Just a  -> return (Edge eid rhs a)
    Nothing -> return (Edge eid rhs [])

data Statement =
    NodeStmt Node
  | EdgeStmt Edge
  | AttributeStmt Attribute
  | AttrStmt Attr
  | SubgraphStmt Graph
  deriving Show

nodeStmt :: Parser Statement
nodeStmt = fmap NodeStmt node

edgeStmt :: Parser Statement
edgeStmt = fmap EdgeStmt edge

attributeStmt :: Parser Statement
attributeStmt = fmap AttributeStmt attribute

attrStmt :: Parser Statement
attrStmt = fmap AttrStmt attr

subgraphStmt :: Parser Statement
subgraphStmt = do
  g <- graph
  case getType g of
    SubgraphType -> return (SubgraphStmt g)
    _            -> fail "expected: subgraph"

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


statement :: Parser Statement
statement =
  (
    try attrStmt
    <|>
    try attributeStmt
    <|>
    try edgeStmt
    <|>
    try subgraphStmt
    <|>
    nodeStmt
  ) <* many (token (string ";" <|> comment))

data GraphType =
    GraphType
  | DigraphType
  | StrictGraphType
  | StrictDigraphType
  | SubgraphType
  deriving Show

graphType :: Parser GraphType
graphType = token $
  fmap (const GraphType)
    (string "graph")
  <|>
  fmap (const DigraphType)
    (string "digraph")
  <|>
  fmap (const SubgraphType)
    (string "subgraph")
  <|>
  fmap (const StrictGraphType)
    (try (token (string "strict") >> string "graph"))
  <|>
  fmap (const StrictDigraphType)
    (try (token (string "strict") >> string "digraph"))

type Subgraph = Graph
type SelfAttr = Attr

data Graph =
  Graph {
      getType :: GraphType
    , getId :: (Maybe ID)
    , getNodes :: [Node]
    , getEdges :: [Edge]
    , getAttributes :: [Attribute]
    , getSelfAttributes :: [SelfAttr]
    , getSubgraph :: [Subgraph]
  }
  deriving (Show)

graph :: Parser Graph
graph = do
  _   <- skipNoise
  tpe <- graphType
  gid <- optional graphId
  _   <- token (char '{')
  sts <- many statement
  _   <- token (char '}')
  _   <- skipNoise
  let (ns, es, as, ss, gs) = pick (reverse sts) ([],[],[],[],[])
  return (Graph tpe gid ns es as ss gs)
    where skipNoise = skipMany (space <|> (comment >> return '\n'))
          pick [] acc = acc
          pick (x:xs) (ns, es, as, ss, gs) =
            pick xs $ case x of
              (NodeStmt n)       -> (n:ns, es, as, ss, gs)
              (EdgeStmt e)       -> (ns, e:es, as, ss, gs)
              (AttributeStmt a ) -> (ns, es, a:as, ss, gs)
              (AttrStmt s)       -> (ns, es, as, s:ss, gs)
              (SubgraphStmt g)   -> (ns, es, as, ss, g:gs)

example :: String
example = [r|
// This is a test Graph
 strict digraph {
  awesome = yes # no rly!
  node [test = passed];
  graph [a = 1, b = 2];
  edge [a = important, b = "also important"]
  a -- b
  c -> d
  /* here we have
   * a multiline comment
   */
  n1 [color = red]
  d -> a
  subgraph test {
    g -- o -- o -- d
  }
}
/* The End.
 * Bye now!
 */
|]
