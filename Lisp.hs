import System.Environment
import Parser.Parse(readExpr)
import Engine.Eval(eval)
import LispError

main :: IO ()
main = do
     args      <- getArgs
     evaluated <- return $ fmap show $ readExpr (args !! 0) >>= eval
     putStrLn $ extractValue $ trapError evaluated
