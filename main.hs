import Parser (parse)
import Builtins (runEval)

main = do
  putStr "> "
  input <- getLine
  print $ parse $ "(" ++ input ++ ")"
  print $ runEval $ parse input
  main
