-- #!/usr/local/bin/runhaskell
-- parsingText.hs


import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr
import Text.ParserCombinators.Parsec.Language (emptyDef)
import qualified Text.ParserCombinators.Parsec.Token as P

lexer = P.makeTokenParser emptyDef
identifier = P.identifier lexer
parens = P.parens lexer
reservedOp = P.reservedOp lexer
natural = P.natural lexer

data Tag = MkTag String deriving (Show)

parseTag :: Parser Tag
parseTag = do
    char '<'
    x <- identifier
    char '>'
    return (MkTag x)

parseDiv :: Parser Tag
parseDiv = do 
    string "<div>" 
    return (MkTag "div")

parseParens = do
    x <- parens identifier
    return x

letter_digit :: Parser Char
letter_digit = do
    x <- letter <|> digit
    return x

bag_bog :: Parser String
bag_bog = do  
    xs <- string "bag" <|> string "bog"
    return xs

bag_bog_try :: Parser String
bag_bog_try = do  
    xs <- try (string "bag") <|> string "bog"
    return xs

varname :: Parser String
varname = do  
    x <- letter
    xs <- many (letter <|> digit)
    return (x:xs)

expr = buildExpressionParser optable term <?> "expression"

term =  parens expr 
    <|> natural
    <?> "simple expression"

optable =
    [ [prefix "-" negate, prefix "+" id ]
    , [postfix "++" (+1)]
    , [binary "*" (*) AssocLeft, binary "/" (div) AssocLeft ]
    , [binary "+" (+) AssocLeft, binary "-" (-)   AssocLeft ]
    ]

binary  name fun assoc = Infix (do{ reservedOp name; return fun }) assoc
prefix  name fun       = Prefix (do{ reservedOp name; return fun })
postfix name fun       = Postfix (do{ reservedOp name; return fun })

main = do
    putStrLn "-------------- TEST parseTag"
    parseTest parseTag ("<list>")
    putStrLn "-------------- TEST parseDiv"
    parseTest parseDiv ("<div>")
    putStrLn "-------------- TEST char"
    parseTest (char 'c') "cons"
    parseTest (char 'c') "dons"
    putStrLn "-------------- TEST parseParens"
    parseTest parseParens "(fok)"
    parseTest parseParens "(fok"
    putStrLn "-------------- TEST letter_digit"
    parseTest letter_digit "a1"
    parseTest letter_digit "1a"
    putStrLn "-------------- TEST bag_bog"
    parseTest bag_bog "big"
    parseTest bag_bog "bog"
    putStrLn "-------------- TEST bag_bog_try"
    parseTest bag_bog_try "bog"
    putStrLn "-------------- TEST varname"
    parseTest varname "variable1"
    parseTest varname "1variable"
    putStrLn "-------------- TEST expr"
    parseTest expr "2+2"
    parseTest expr "2*3"
    parseTest expr "7++"
    parseTest expr "(2+3)*5"
    parseTest expr "2+3*5"
    parseTest expr "2+3/5"


{--------------- Some basic Parsec library parsers

    (char\; “?”) — (char) is applied to a character, and it gives a parser that matches that character
    (letter) — matches any letter
    (digit) — matches any digit
    (string) — matches a string of characters
    (stringLiteral\; “xyz*”) — matches the string argument
    (many\; p) — matches 0 or more occurrences of parser (p)
    (many1\; p) — matches 1 or more occurrences of parser (p)

----------------}
