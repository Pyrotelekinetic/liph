import Parser (parse)
import Eval (runEval)

main = do
  putStr "$ "
  input <- getLine
  let code = "(" ++ input ++ ")"
  putStrLn $ "> " ++ show (runEval . parse $ code)
  main
