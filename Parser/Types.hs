module Parser.Types where
-- define Lisp Abstract Syntax Tree using Abstract Data Types
data LispAST = Ident String
              | List [LispAST]
              | DottedList [LispAST] LispAST
              | Number Integer
              | String String
              | Bool Bool
              deriving(Show, Eq)

getInteger (Number number) = number
getString (String string) = string
getBool   (Bool     bool) = bool

-- allow for inspection of LispVal instances as strings
-- unwordsList :: [LispVal] -> String
-- unwordsList  = unwords . map showVal

-- instance Show LispVal where show = showAST

-- showAST :: LispVal -> String
-- showAST (String  string)        = "String " ++ show string
-- showAST (Atom   atom)           = "Atom " ++ show atom
-- showAST (Number number)         = "Number " ++ show number
-- showAST (Bool   bool)           = "Bool " ++ show bool
-- showAST (List   list)           = "List " ++ (show list) ++ "\n"
-- showAST (DottedList head tail)  = "DottedList " ++ (show head) ++ " " ++ (show tail)
