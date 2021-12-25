module Engine.Eval where

import Parser.Types
import Engine.Apply

eval :: LispAST -> LispAST
eval val@(String _) = val
eval val@(Number _) = val
eval val@(Bool   _) = val
eval (List [Ident "quote", val]) = val

-- recursive definition of evaluate that can handle
-- lists of lists
eval (List (Ident func : args)) = apply (Ident func) $ map eval args
