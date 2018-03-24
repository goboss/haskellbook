module Exercises.Parsers.DOT where

import Data.Monoid ((<>))
import Control.Applicative ((<|>))
import Text.Trifecta

data GraphType = Graph | Digraph deriving Show
type GraphId = String

graphType :: Parser GraphType
graphType =
  fmap (const Graph) (string "graph")
  <|>
  fmap (const Digraph) (string "digraph")

graphId :: Parser (Maybe GraphId)
graphId = optional (stringId <|> numeralId <|> quotedId <|> htmlId)
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
