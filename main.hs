import Parser (parse)
import Eval (runEval)

main = do
--  putStr "$ "
  input <- getLine
  putStrLn $ "$ " ++ input
  case parse input of
    Left e -> putStrLn e
    Right input' -> do
      putStrLn $ "parsed as: " ++ show input'
      putStrLn $ "> " ++ show (runEval input')
  main
