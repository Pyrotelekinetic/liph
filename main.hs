import Parser (parse)
import Eval (runEval)

main = do
  putStr "> "
  input <- getLine
  print $ parse $ "(" ++ input ++ ")"
--  print $ parse input
  print $ runEval $ parse $ "(" ++ input ++ ")"
  main
