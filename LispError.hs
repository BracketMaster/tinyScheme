module LispError where

import Control.Monad.Except(catchError)
import Parser.Types ( LispAST )
import Text.Parsec (ParseError)
import Numeric (showEFloat)

data LispError = NumArgs Integer [LispAST]
               | Type String LispAST
               | Parser ParseError
               | BadSpecialForm String LispAST
               | NotFunction String LispAST
               | UnboundVar String LispAST
               | Default String

instance Show LispError where show = showError
type ThrowsError = Either LispError

trapError action = catchError action (return . show)

extractValue :: ThrowsError a -> a
extractValue (Right val) = val

showError :: LispError -> [Char]
showError error_type =
  "Error of type: " ++ name error_type ++ ".\n" ++
  case error_type of
    (NumArgs       expected found) -> inequality expected found
    (Type          expected found) -> inequality expected found
    (Parser                 error) -> show error
    (BadSpecialForm  message form) -> message ++ ": " ++ show form
    (NotFunction     message func) -> message ++ ": " ++ show func
    (UnboundVar   message varname) -> message ++ ": " ++ show varname
    (Default              message) -> message
    where
      name (NumArgs        _ _) = "NumArgs"
      name (Type           _ _) = "Type"
      name (Parser           _) = "Parser"
      name (BadSpecialForm _ _) = "BadSpecialForm"
      name (NotFunction    _ _) = "NotFunction"
      name (UnboundVar     _ _) = "UnboundVar"
      name (Default          _) = "Default"

      inequality expected found = "Expected : " ++ name error_type
                                  ++ "=" ++ show expected
                                  ++ "but found : " ++ show found
