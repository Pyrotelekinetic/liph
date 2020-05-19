import Parser (parse)
import Eval (runEval)

main = do
--  putStr "$ "
  input <- getLine
  putStrLn $ "$ " ++ input
  putStrLn $ "parsed as: " ++ show (parse input)
  putStrLn $ "> " ++ show (runEval . parse $ input)
  main
