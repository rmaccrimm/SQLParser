module Main where

import Text.Parsec
import Text.Parsec.String
import Data.Char
import Data.List

data SelectQuery = WithFrom SelectClause FromClause
                 | WithoutFrom SelectClause
  deriving Show

data Name = Name String | ReName String String
  deriving Show

data SelectClause = SelectClause [Name]
  deriving Show

data FromClause = FromClause [Name]
  deriving Show

data FromItem = FromTable Name
  deriving Show

main :: IO ()
main = do
  input <- getLine
  parseTest selectQuery input

whiteSpace :: Parser Char
whiteSpace = endOfLine <|> space <|> tab

followedBy :: Parser a -> Parser b -> Parser a
followedBy a b = do { x <- a; b; return x }

listOf :: Parser a -> Parser [a]
listOf p = p' `sepBy` comma 
  where p' = p `followedBy` many whiteSpace
        comma = char ',' `followedBy` many whiteSpace

keyword :: Parser String
keyword = select <|> from

select :: Parser String
select = try $ string "select"

from :: Parser String
from = try $ string "from"        

identifier :: Parser String
identifier = do
  notFollowedBy keyword
  fst <- letter
  end <- many $ satisfy (\a -> isAlphaNum a || a == '_')
  return (fst:end)

name :: Parser Name
name = Name <$> (join <$> identifiers)
  where join = intercalate "."
        identifiers = identifier `sepBy1` char '.'

selectClause :: Parser SelectClause
selectClause = do
  select `followedBy` many whiteSpace
  cols <- listOf name
  return (SelectClause cols)

fromClause :: Parser FromClause
fromClause = do
  from `followedBy` many whiteSpace
  tables <- listOf name
  return (FromClause tables)

selectQuery :: Parser SelectQuery
selectQuery = do
  select <- selectClause
  optional fromClause
  return (WithoutFrom select)
