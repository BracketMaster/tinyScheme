{-# LANGUAGE FlexibleContexts #-}
module Engine.Reduce where
import LispError
import Control.Monad.Except(throwError)

import Parser.Types

reduce :: LispAST -> [LispAST] -> ThrowsError LispAST
reduce func args = maybe (throwError error) ($ args) (lookup func builtins)
  where error = NotFunction "Did not recognize at primitive function" func

builtins :: [(LispAST, [LispAST] -> ThrowsError LispAST)]
builtins  = [
  (Ident "+", numericOp (+)),
  (Ident "-", numericOp (-)),
  (Ident "*", numericOp (*)),
  (Ident "/", numericOp div)]

numericOp :: (Integer -> Integer -> Integer) -> [LispAST] -> ThrowsError LispAST
numericOp op ((Number firstnum):rest) = return $ Number (foldl op firstnum (map getInteger rest))
numericOp op [single_val] = throwError $ NumArgs 2 [single_val]
numericOp op [] = throwError $ NumArgs 2 []
