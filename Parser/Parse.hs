module Parser.Parse where

import LispError
import Control.Monad.Except(throwError)
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

readExpr :: String -> ThrowsError LispAST
readExpr input =    case parse_tree of
                        Left  err -> throwError $ Parser  err
                        Right val -> return val
                    where parse_tree = parse parseExpr "lisp" input
