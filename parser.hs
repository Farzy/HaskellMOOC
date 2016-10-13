-- week4: Parser

import Text.ParserCombinators.Parsec
import qualified Text.ParserCombinators.Parsec.Token as P
import Text.ParserCombinators.Parsec.Language

data Tag = MkTag String deriving Show

parseTag :: Parser Tag
parseTag =
    do
        char '<'
        x <- identifier
        char '>'
        return (MkTag x)

parseDiv :: Parser Tag
parseDiv =
    do
        string "<div>"
        return (MkTag "div")

letter_digit :: Parser Char
letter_digit =
    do
        x <- letter <|> digit
        return x

digit_in_parens :: Parser Char
digit_in_parens =
    do
        x <- parens digit
        return x

bag_bog :: Parser String
bag_bog =
  do  xs <- string "bag" <|> string "bog"
      return xs

bag_bog_try :: Parser String
bag_bog_try =
  do  xs <- try (string "bag") <|> string "bog"
      return xs

varname :: Parser String
varname =
  do  x <- letter
      xs <- many (letter <|> digit)
      return (x:xs)


lexer = P.makeTokenParser emptyDef
identifier = P.identifier lexer
parens = P.parens lexer
