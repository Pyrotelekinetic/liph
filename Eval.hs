{-# LANGUAGE LambdaCase #-}

module Eval where

import Parser (Sexp (..), Table, State)

import Debug.Trace

eval :: State -> State
--eval table = \case
eval (t, e) = trace ("calling eval on\n    " ++ show e ++ "\nwith\n    " ++ show t ++ "\n\n") $ case e of
  Func f := x -> eval $ f (t, x)
  Atom a := x -> eval (t, (exists a t) := x)
  x := y -> (ty, x' := y')
    where
    (tx, x') = eval (t, x)
    (ty, y') = eval (tx, y)
  x -> (t, x)

exists :: String -> [Table] -> Sexp
exists s = \case
  [] -> Atom s
  (n, f) : ts
    | n == s -> f
    | otherwise -> exists s ts

-- sums a list of Ints
plusL :: State -> State
--plusL table e = case eval table e of
plusL (t, e) = trace ("calling eval on:\n    " ++ show e ++ "\nwith:\n    " ++ show t) $ case eval (t, e) of
  (t', Int x := Int y) -> (t', Int (x + y))
  (t', Int x := Nil) -> (t, Int x)
  (t', Int x := y) -> plusL (t', Int x := y')
    where
    y' = snd $ plusL (t', y)
  _ -> (t, Atom "Type Error: '+' takes Ints")

-- subtracts two Ints
minusL :: State -> State
minusL (t, e) = case eval (t, e) of
  (t', Int x := Int y := Nil) -> (t', Int (x - y))
  _ -> (t, Atom "Type Error: '-' takes two Ints")

-- multiplies two Ints
multiplyL :: State -> State
multiplyL (t, e) = case eval (t, e) of
  (t', Int x := Int y) -> (t', Int (x * y))
  (t', Int x := Nil) -> (t', Int x)
  (t', Int x := y) -> multiplyL (t', Int x := y')
    where
    y' = snd $ multiplyL (t', y)
  _ -> (t, Atom "Type Error: '*' takes Ints")

-- divides two Ints
divideL :: State -> State
divideL (t, e) = case eval (t, e) of
  (t', Int x := Int 0 := Nil) -> (t', Atom "Please do not divide by zero")
  (t', Int x := Int y := Nil) -> (t', Int (div x y))
  _ -> (t, Atom "Type Error: '/' takes two Ints")

-- negates one Int
negativeL :: State -> State
negativeL (t, e) = case eval (t, e) of
  (t', Int x) -> (t', Int (- x))
  _ -> (t, Atom "Type Error: 'neg' takes one Int")

-- defines a new variable
letL :: State -> State
--letL t = \case
letL (t, e) = case (t, e) of
  (t, Atom n := Atom "=" := x := Atom "in" := e) -> (t, e')
    where
    t' = (n, x) : t
    e' = snd $ eval (t', e)
  (t, Atom n := Atom "=" := x := e) -> (t', Nil)
    where
    t' = (n, x) : t
  _ -> (t, Atom "let Error")

-- TODO --
-- defines a function
defunL :: State -> State
defunL (t, e) = trace ("calling defun on\n    " ++ show e) $ case e of
  Atom n := xs := d -> (parseFun t n xs d, Nil)
    where
    parseFun :: [Table] -> String -> Sexp -> Sexp -> [Table]
    parseFun t n xs = \case
      (Atom f := xs) -> trace "pattern match success!" $ case exists f t of
        Func fL -> trace "defun success!" $ (f, Func fL) : t
        _ -> trace "defun fail" t
      _ -> trace "pattern match failed\n" t

arithmetic :: [Table]
arithmetic =
  [ ("+", Func plusL)
  , ("-", Func minusL)
  , ("*", Func multiplyL)
  , ("/", Func divideL)
  , ("neg", Func negativeL)
  ]

builtins :: [Table]
builtins = ("let", Func letL) : ("defun", Func defunL) : arithmetic

runEval :: Sexp -> Sexp
runEval x = snd $ eval (builtins, x)
