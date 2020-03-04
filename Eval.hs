{-# LANGUAGE LambdaCase #-}

module Eval where

import Parser (Sexp (..), Table)

import Debug.Trace

eval :: [Table] -> Sexp -> ([Table], Sexp)
eval t = \case
--eval t e = trace ("calling eval on\n    " ++ show e ++ "\nwith\n    " ++ show t ++ "\n\n") $ case e of
  Func f := x -> case f t x of
    (result, tableResult) -> eval result tableResult
  Atom a := x -> eval t $ (exists a t) := x
  x := y -> (ty, x' := y')
    where
    (tx, x') = eval t x
    (ty, y') = eval tx y
  x -> (t, x)
  where
  exists :: String -> [Table] -> Sexp
  exists s = \case
    [] -> Atom s
    (n, f) : ts
      | n == s -> f
      | otherwise -> exists s ts

-- sums a list of Ints
plusL :: [Table] -> Sexp -> ([Table], Sexp)
plusL t e = case eval t e of
--plusL t e = trace ("calling eval on:\n    " ++ show e ++ "\nwith:\n    " ++ show t) $ case eval t e of
  (t', Int x := Int y) -> (t', Int (x + y))
  (t', Int x := Nil) -> (t, Int x)
  (t', Int x := y) -> plusL t' $ Int x := y'
    where
    (_, y') = plusL t' y
  _ -> (t, Atom "Type Error: '+' takes Ints")

-- subtracts two Ints
minusL :: [Table] -> Sexp -> ([Table], Sexp)
minusL t e = case eval t e of
  (t', Int x := Int y := Nil) -> (t', Int (x - y))
  _ -> (t, Atom "Type Error: '-' takes two Ints")

-- multiplies two Ints
multiplyL :: [Table] -> Sexp -> ([Table], Sexp)
multiplyL t e = case eval t e of
  (t', Int x := Int y) -> (t', Int (x * y))
  (t', Int x := Nil) -> (t', Int x)
  (t', Int x := y) -> multiplyL t' $ Int x := y'
    where
    (_, y') = multiplyL t' y
  _ -> (t, Atom "Type Error: '*' takes Ints")

-- divides two Ints
divideL :: [Table] -> Sexp -> ([Table], Sexp)
divideL t e = case eval t e of
  (t', Int x := Int 0 := Nil) -> (t', Atom "Please do not divide by zero")
  (t', Int x := Int y := Nil) -> (t', Int (div x y))
  _ -> (t, Atom "Type Error: '/' takes two Ints")

-- negates one Int
negativeL :: [Table] -> Sexp -> ([Table], Sexp)
negativeL t e = case eval t e of
  (t', Int x) -> (t', Int (- x))
  _ -> (t, Atom "Type Error: 'neg' takes one Int")

-- defines a new variable
letL :: [Table] -> Sexp -> ([Table], Sexp)
letL t = \case
  Atom n := Atom "=" := x := Atom "in" := e -> (t, e')
    where
    t' = (n, x) : t
    (_, e') = eval t' e
  Atom n := Atom "=" := x := e -> (t', Nil)
    where
    t' = (n, x) : t
  _ -> (t, Atom "let Error")

-- defines a function
defunL :: [Table] -> Sexp -> ([Table], Sexp)
defunL t = \case
--defunL t e = trace ("calling defun on\n    " ++ show e) $ case e of
  Atom n := xs := d -> (parseFun t n xs d, Nil)
    where
    parseFun :: [Table] -> String -> Sexp -> Sexp -> [Table]
    parseFun t n xs = \case
      (Func f1 := xs) -> (n, Func f1) : (n, Func f1) : t
      _ -> trace "pattern match failed\n" t

stripNil :: Sexp -> Sexp
stripNil = \case
  x := Nil -> x
  Nil := x -> x
  x := y -> (stripNil x) := (stripNil y)
  x -> x

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
runEval x = stripNil $ snd $ eval builtins x
