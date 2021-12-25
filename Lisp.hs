import System.Environment
import Parser.Parse(readExpr)
import Engine.Eval(eval)

main :: IO ()
main = do
        args <- getArgs
        (print . eval . readExpr . head) args
