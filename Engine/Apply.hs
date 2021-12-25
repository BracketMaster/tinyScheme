module Engine.Apply where

import Parser.Types

apply :: LispAST -> [LispAST] -> LispAST
apply func args = maybe (Bool False) ($ args) (lookup func builtins)

builtins :: [(LispAST, [LispAST] -> LispAST)]
builtins  = [
  (Ident "+", numericOp (+)),
  (Ident "-", numericOp (-)),
  (Ident "*", numericOp (*)),
  (Ident "/", numericOp div)]

numericOp :: (Integer -> Integer -> Integer) -> [LispAST] -> LispAST
numericOp op ((Number firstnum):rest) = Number (foldl op firstnum (map getInteger rest))
