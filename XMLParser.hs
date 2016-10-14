-- module XMLParser ( parseXML )
-- Author: Farzad FARID
-- Date: 2016/10/13
-- Haskell MOOC week 4

module XMLParser
where
import Text.ParserCombinators.Parsec
import qualified Text.ParserCombinators.Parsec.Token as P
import Text.ParserCombinators.Parsec.Language
import Data.List ( intercalate )

xml_header =  "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n"

-- Let's say these are the only authorized characters in an XML tag
tag_chars = concat [['a'..'z'], ['A'..'Z'], "-"]

parseXML :: String -> String
parseXML str = run_parser xmlParser str

run_parser :: Parser a -> String -> a
run_parser p str =  case parse p "Inline XML" str of
    Left err -> error $ "parse error at " ++ (show err)
    Right val  -> val  

xmlParser :: Parser String 
xmlParser =
    xmldoc_parser
    <?> "Parse error"

xmldoc_parser = do
    string xml_header
    spaces
    b <- tag_parser
    spaces
    return $ "OK found XML header\nBODY: " ++ b

quoted_string = do
    s <- between (char '"') (char '"') (many $ noneOf "\"")
    return s

content_parser = try tag_parser <|> quoted_string 

tag_parser = do
    -- XXX: This does not validate the authorized characters in a tag 
    i <- angles $ many $ noneOf ">" -- E.g.: "div class=\"xx\""
    let tag = head $ words i        -- Extract the tag. E.g.: "div"
        attrs = drop (length tag) i -- Extract the rest. E.g.: " class=\"xx\""
    tt <- many content_parser 
    angles $ string ('/':tag) -- Same closing tag
    spaces
    return $ "\n  TAG: '" ++ tag ++ "', ATTRS: '" ++ attrs ++ "' --> " ++ concat tt

-- tag_identifier = do
--     first <- letter
--     rest <- many $ oneOf tag_chars
--     return $ first:rest

lexer       = P.makeTokenParser emptyDef

parens          = P.parens lexer    
brackets        = P.brackets lexer    
braces          = P.braces lexer    
angles          = P.angles lexer    
commaSep        = P.commaSep lexer
whiteSpace      = P.whiteSpace lexer    
symbol          = P.symbol lexer    
identifier      = P.identifier lexer
integer         = P.integer lexer    
stringLiteral   = P.stringLiteral lexer 
