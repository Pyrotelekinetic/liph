import Parser (parse)
import Evaluator (runEval)

main = do
  putStr "> "
  input <- getLine
  print $ parse $ "(" ++ input ++ ")"
  print $ runEval $ parse input
  main
