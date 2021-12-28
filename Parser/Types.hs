module Parser.Types where

import Data.Data
-- define Lisp Abstract Syntax Tree using Abstract Data Types
data LispAST = Ident String
              | List [LispAST]
              | DottedList [LispAST] LispAST
              | Number Integer
              | String String
              | Bool Bool
              deriving(Show, Eq)

getInteger :: LispAST -> Integer
getInteger (Number number) = number
getString :: LispAST -> String
getString (String string) = string
getBool :: LispAST -> Bool
getBool   (Bool     bool) = bool
