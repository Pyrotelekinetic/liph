{-# LANGUAGE LambdaCase #-}

module Eval where

import Parser (Sexp (..), Table, State)
import Debug.Trace

table :: State -> Table
table = fst

sexp :: State -> Sexp
sexp = snd

map' :: (Sexp -> Sexp) -> Sexp -> Sexp
map' f NilL = NilL
map' f (x := y) = (f x) := (map' f y)
map' f x = f x

eval :: State -> State
eval (t, e) = case e of
  ErrorL err -> ([], NilL)
  FuncL f := x -> f (t, x)
--  AtomL "defun" := y -> case y of
--    (AtomL fn := xs := d := NilL) ->
--    _ -> ([], ErrorL "Syntax Error: invalid function definition")
  AtomL a -> eval (t, getBind (t, AtomL a))
--  AtomL a := x -> eval (t, getBind (t, AtomL a) := x)
  x := ys -> (t, sexp (eval (t, x' := ys)))
    where
    (tx, x') = eval (t, x)
  x -> (t, x)

{-
-- (defun f (x1 x2) (+ x1 x2))
defunL (AtomL "f" := (x1 := x2 := NilL) := d := NilL)
defunL (f := xs := d := NilL)
-}

--initState :: State -> State
--initState (t, e) = case e of
--  AtomL "defun" := AtomL fn := xs := d := NilL -> ((fn, FuncL f) : t, e)
--  x := ys -> -- recurse

getBind :: State -> Sexp
getBind = \case
  ([], AtomL s) -> ErrorL "Error: exhasted bindings"
  ((n, f) : ts, AtomL s)
    | n == s -> f
    | otherwise -> getBind (ts, AtomL s)

evalList :: State -> State
evalList (t, xs) = (t, map' (\x -> sexp $ eval (t, x)) xs)


-- Arithmetic --

-- sums a list of IntLs
plusL :: State -> State
plusL x = case (evalList x) of
  (t', IntL n1 := IntL n2) -> (t', IntL (n1 + n2))
  (t', IntL n := NilL) -> (t', IntL n)
  (t', IntL n1 := n2) -> plusL (t', IntL n1 := (sexp $ plusL (t', n2)))
  (t', z) -> (t', ErrorL $ "Type Error: '+' takes Ints,\n  " ++ show z ++ " do not have type Int")

-- subtracts two IntLs
minusL :: State -> State
minusL x = case evalList x of
  (t', IntL n1 := IntL n2 := NilL) -> (t', IntL (n1 - n2))
  (t', z) -> (t', ErrorL $ "Type Error: '-' takes two Ints,\n  " ++ show z ++ " do not have type Int")

-- multiplies two IntLs
multiplyL :: State -> State
multiplyL x = case evalList x of
  (t', IntL n1 := IntL n2) -> (t', IntL (n1 * n2))
  (t', IntL n := NilL) -> (t', IntL n)
  (t', IntL n1 := n2) -> multiplyL (t', IntL n1 := (sexp $ multiplyL (t', n2)))
    where
    n2' = sexp $ multiplyL (t', n2)
  (t', z) -> (t', ErrorL $ "Type Error: '*' takes IntLs,\n  "++ show z ++ " do not have type Int")

-- divides two IntLs
divideL :: State -> State
divideL x = case evalList x of
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

-- defines a let expression
letL :: State -> State
letL = \case
  (t, AtomL n :=  x := e := NilL) -> (t', e')
    where
    t' = (n, x) : t
    e' = sexp $ eval (t', e)
  (t, _) -> (t, ErrorL "Syntax Error: Invalid let expression")

extend :: Sexp -> Sexp -> Table -> Maybe Table
extend NilL NilL t = Just t
extend NilL vs _ = Nothing
extend xs NilL _ = Nothing
extend (AtomL x := xs) (v := vs) t = ((x, v) :) <$> extend xs vs t

-- defines a lambda expression
lambdaL :: State -> State
lambdaL = \case
  (t, xs := ds := NilL) -> eval (t, FuncL fn)
    where
      fn (t', es) = let vs = sexp $ evalList (t', es) in
        case extend xs vs t of
          Just t -> eval (t, ds)
          Nothing -> ([], ErrorL "Error: incorrect number of args in lambda expression")
  (t, _) -> (t, ErrorL "Syntax Error: invalid lambda expression")

-- defines a recursive lambda expression
fixL :: State -> State
fixL = \case
  (t, AtomL f := xs := d := NilL) -> eval (t, FuncL fn)
    where
    fn (t', es) = let vs = sexp $ evalList (t', es) in
      case extend xs vs ((f, FuncL fn) : t) of
        Just t -> eval (t, d)
        Nothing -> ([], ErrorL "Error: incorrect number of args in fix expression")
  _ -> ([], ErrorL "Error: invalid fix expression")

lets :: Table
lets =
  [ ("let", FuncL letL)
  , ("lambda", FuncL lambdaL)
  , ("fix", FuncL fixL)
  ]


-- Bool --

equalsL :: State -> State
equalsL (t, x) = case x of
  a := NilL -> ([], ErrorL "Type Error: '=' takes two arguments")
  a := b -> case a == b of
    True -> (t, BoolL True)
    False -> (t, BoolL False)

andL :: State -> State
andL (t, x) = case stripNilL $ sexp $ eval (t, x) of
  BoolL True := BoolL True -> (t, BoolL True)
  BoolL _ := BoolL _ -> (t, BoolL False)
  _ -> (t, ErrorL "Type Error: 'and' takes two Bools")

orL :: State -> State
orL (t, x) = case stripNilL $ sexp $ eval (t, x) of
  BoolL True := BoolL _ -> (t, BoolL True)
  BoolL _ := BoolL True -> (t, BoolL True)
  _ -> (t, ErrorL "Type Error: 'or' takes two Bools")

xorL :: State -> State
xorL (t, x) = case stripNilL $ sexp $ eval (t, x) of
  BoolL True := BoolL False -> (t, BoolL True)
  BoolL False := BoolL True -> (t, BoolL True)
  BoolL _ := BoolL _ -> (t, BoolL False)
  _ -> (t, ErrorL "Type Error: 'xor' takes two Bools")

notL :: State -> State
notL (t, x) = case stripNilL $ sexp $ eval (t, x) of
  BoolL True -> (t, BoolL False)
  BoolL False -> (t, BoolL True)
  _ -> (t, ErrorL "Type Error: 'not' takes one Bool")

ifL :: State -> State
ifL (t, p := d := e) = case stripNilL $ sexp $ eval (t, p) of
  BoolL True -> eval (t, d)
  BoolL False -> eval (t, e)
  _ -> (t, ErrorL "Type Error: 'if' takes one Bool and one expression")

bools :: Table
bools =
  [ ("#t", BoolL True)
  , ("#T", BoolL True)
  , ("#f", BoolL False)
  , ("#F", BoolL False)
  , ("=", FuncL equalsL)
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
