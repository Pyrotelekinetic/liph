import Parser (parse)
import Eval (runEval)

main = do
  putStr "> "
  input <- getLine
  let code = "(" ++ input ++ ")"
--  print $ parse code
  print $ runEval $ parse code
  main
