module Engine.Eval where

import LispError
import Parser.Types
import Engine.Reduce

eval :: LispAST -> ThrowsError LispAST
eval val@(String _) = return val
eval val@(Number _) = return val
eval val@(Bool   _) = return val
eval (List [Ident "quote", val]) = return val

-- recursive definition of evaluate that can handle
-- lists of lists
-- eval (List (Ident func : args)) = apply (Ident func) $ map eval args
eval (List (Ident func : args)) = mapM eval args >>= reduce (Ident func)
