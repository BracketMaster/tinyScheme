module Parser.Parse where

import Parser.Types
import Text.ParserCombinators.Parsec hiding (spaces)

symbol :: Parser Char
symbol = oneOf "!#$%&|*+-/:<=>?@^_~"

spaces :: Parser ()
spaces =  skipMany1 $ char ' '

parseNumber :: Parser LispAST
parseNumber  = fmap (Number . read) $ many1 digit

parseString :: Parser LispAST
parseString =   do
                  char '"'
                  x <- many (noneOf "\"")
                  char '"'
                  return $ String x

parseAtom :: Parser LispAST
parseAtom =  do
              first <- letter <|> symbol
              rest  <- many (letter <|> digit <|> symbol)
              let ident = first:rest
              return $ case ident of
                        "#t" -> Bool True
                        "#f" -> Bool False
                        _    -> Ident ident

parseQuoted :: Parser LispAST
parseQuoted  = do
                char '\''
                x <- parseExpr
                return $ List [Ident "quote", x]

parseList :: Parser LispAST
parseList  = fmap List $ sepBy parseExpr spaces

parseDottedList :: Parser LispAST
parseDottedList  = do
                    head <- endBy parseExpr spaces
                    tail <- char '.' >> spaces >> parseExpr
                    return $ DottedList head tail

parseExpr :: Parser LispAST
parseExpr  = parseAtom
          <|> parseString
          <|> parseNumber
          <|> parseQuoted
          <|> do
                char '('
                x <- try parseList <|> parseDottedList
                char ')'
                return x

readExpr :: String -> LispAST
readExpr input =    case parsed_symbol of
                        Left  err -> String $ "No match: " ++ show err
                        Right val -> val
                    where parsed_symbol = parse parseExpr "lisp" input
