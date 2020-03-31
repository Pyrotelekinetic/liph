{-# LANGUAGE LambdaCase #-}

module Eval where

import Parser (Sexp (..), Table, State)

table :: State -> Table
table = fst

sexp :: State -> Sexp
sexp = snd

eval :: State -> State
eval (t, e) = case e of
  FuncL f := x -> f (t, x)
  AtomL a := x -> eval (t, getBind (t, AtomL a) := x)
  x := y -> (ty, x' := y')
    where
    (tx, x') = eval (t, x)
    (ty, y') = eval (tx, y)
  x -> (t, x)

getBind :: State -> Sexp
getBind = \case
  ([], AtomL s) -> ErrorL "Error: exhasted bindings"
  ((n, f) : ts, AtomL s)
    | n == s -> f
    | otherwise -> getBind (ts, AtomL s)


-- Arithmetic --

-- sums a list of IntLs
plusL :: State -> State
plusL x = case eval x of
  (t', IntL n1 := IntL n2) -> (t', IntL (n1 + n2))
  (t', IntL n := NilL) -> (t', IntL n)
  (t', IntL n1 := n2) -> plusL (t', IntL n1 := (sexp $ plusL (t', n2)))
  (t', z) -> (t', ErrorL $ "Type Error: '+' takes Ints,\n  " ++ show z ++ " do not have type Int")

-- subtracts two IntLs
minusL :: State -> State
minusL x = case eval x of
  (t', IntL n1 := IntL n2 := NilL) -> (t', IntL (n1 - n2))
  (t', z) -> (t', ErrorL $ "Type Error: '-' takes two Ints,\n  " ++ show z ++ " do not have type Int")

-- multiplies two IntLs
multiplyL :: State -> State
multiplyL x = case eval x of
  (t', IntL n1 := IntL n2) -> (t', IntL (n1 * n2))
  (t', IntL n := NilL) -> (t', IntL n)
  (t', IntL n1 := n2) -> multiplyL (t', IntL n1 := (sexp $ multiplyL (t', n2)))
    where
    n2' = sexp $ multiplyL (t', n2)
  (t', z) -> (t', ErrorL $ "Type Error: '*' takes IntLs,\n  "++ show z ++ " do not have type Int")

-- divides two IntLs
divideL :: State -> State
divideL x = case eval x of
  (t', IntL _ := IntL 0 := NilL) -> (t', ErrorL "Please do not divide by zero")
  (t', IntL n1 := IntL n2 := NilL) -> (t', IntL (div n1 n2))
  (t', z) -> (t', ErrorL $ "Type Error: '/' takes two IntLs,\n  " ++ show z ++ " do not have type Int")

-- negates one IntL
negativeL :: State -> State
negativeL x = case eval x of
  (t', IntL n) -> (t', IntL (- n))
  (t', z) -> (table x, ErrorL $ "Type Error: 'neg' takes one IntL\n  " ++ show z ++ " does not have type Int")

arithmetics :: Table
arithmetics =
  [ ("+", FuncL plusL)
  , ("-", FuncL minusL)
  , ("*", FuncL multiplyL)
  , ("/", FuncL divideL)
  , ("neg", FuncL negativeL)
  ]


-- Let --

-- defines a new variable
letL :: State -> State
letL = \case
  (t, AtomL n := AtomL "=" := x := AtomL "in" := e) -> (t', e')
    where
    t' = (n, x) : t
    e' = sexp $ eval (t', e)
  (t, AtomL n := AtomL "=" := x := e) -> ((n, x) : t, NilL)
  (t, _) -> (t, ErrorL "let Error")

lambdaL :: State -> State
lambdaL = \case
  (t, AtomL x := d) -> (t, FuncL f)
    where
    f (t', e) =
      let v = sexp $ eval (t', e) in
        eval ((x, v) : t, d)
  (t, _) -> (t, ErrorL "error")

lets :: Table
lets =
  [ ("let", FuncL letL)
  , ("lambda", FuncL lambdaL)
  ]


-- Bool --

andL :: State -> State
andL (t, x) = case stripNilL $ snd $ eval (t, x) of
  BoolL True := BoolL True -> (t, BoolL True)
  BoolL _ := BoolL _ -> (t, BoolL False)
  _ -> (t, ErrorL "Type Error: 'and' takes two Bools")

orL :: State -> State
orL (t, x) = case stripNilL $ snd $ eval (t, x) of
  BoolL True := BoolL _ -> (t, BoolL True)
  BoolL _ := BoolL True -> (t, BoolL True)
  _ -> (t, ErrorL "Type Error: 'or' takes two Bools")

xorL :: State -> State
xorL (t, x) = case stripNilL $ snd $ eval (t, x) of
  BoolL True := BoolL False -> (t, BoolL True)
  BoolL False := BoolL True -> (t, BoolL True)
  BoolL _ := BoolL _ -> (t, BoolL False)
  _ -> (t, ErrorL "Type Error: 'xor' takes two Bools")

notL :: State -> State
notL (t, x) = case stripNilL $ snd $ eval (t, x) of
  BoolL True -> (t, BoolL False)
  BoolL False -> (t, BoolL True)
  _ -> (t, ErrorL "Type Error: 'not' takes one Bool")

ifL :: State -> State
ifL (t, p := e) = case stripNilL $ snd $ eval (t, p) of
  BoolL True -> eval (t, e)
  BoolL False -> (t, NilL)
  _ -> (t, ErrorL "Type Error: 'if' takes one Bool and one expression")

bools :: Table
bools =
  [ ("#t", BoolL True)
  , ("#T", BoolL True)
  , ("#f", BoolL False)
  , ("#F", BoolL False)
  , ("and", FuncL andL)
  , ("or", FuncL orL)
  , ("xor", FuncL xorL)
  , ("not", FuncL notL)
  , ("if", FuncL ifL)
  ]


builtins :: Table
builtins = arithmetics ++ bools ++ lets

stripNilL :: Sexp -> Sexp
stripNilL = \case
  x := NilL -> stripNilL x
  NilL := x -> stripNilL x
  x := y -> stripNilL x := stripNilL y
  x -> x

runEval :: Sexp -> Sexp
runEval x = stripNilL $ sexp $ eval (builtins, x)
