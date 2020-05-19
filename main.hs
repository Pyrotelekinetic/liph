import Parser (parse)
import Eval (runEval)

main = do
--  putStr "$ "
  input <- getLine
  let code = "(" ++ input ++ ")"
  putStrLn $ "$ " ++ code
  putStrLn $ "parsed as: " ++ show (parse code)
  putStrLn $ "> " ++ show (runEval . parse $ code)
  main
