{-# LANGUAGE LambdaCase #-}

module Builtins where

import Parser (Sexp (..), Table)

import Control.Monad.Reader

-- evaluates an Sexp
eval :: Sexp -> Reader [Table] Sexp
eval = \case
  Func f := x -> do
    x1 <- eval x
    fx <- f x1
    eval fx

  Atom a := x -> do
    lookupTable <- ask
    eval $ (exists a lookupTable) := x
  x := Atom a -> do
    lookupTable <- ask
    eval $ x := (exists a lookupTable)
  Atom a -> do
    lookupTable <- ask
    return $ exists a lookupTable
  x := y -> do
    x1 <- eval x
    y1 <- eval y
    return $ x1 := y1
  Var v -> eval v
  x -> return x

  where
    exists :: String -> [Table] -> Sexp
    exists s = \case
      [] -> Atom s
      (n, f) : ts
        | n == s -> f
        | otherwise -> exists s ts


plusL :: Sexp -> Reader [Table] Sexp
plusL = \case
  Int x := (Int y) -> return $ Int (x + y)
  Int x := Nil -> return $ Int x
  Int x := y -> do
    y1 <- plusL y
    plusL $ Int x := y1
  _ -> return $ Atom "Type Error: '+' takes Ints"

minusL :: Sexp -> Reader [Table] Sexp
minusL = \case
  Int x := Int y := Nil -> return $ Int (x - y)
  _ -> return $ Atom "Type Error: '-' takes two Ints"

multiplyL :: Sexp -> Reader [Table] Sexp
multiplyL = \case
  Int x := Int y -> return $ Int (x * y)
  Int x := Nil -> return $ Int x
  Int x := y -> do
    y1 <- multiplyL y
    multiplyL $ (Int x) := y1
  _ -> return $ Atom "Type Error: '*' takes Ints"

divideL :: Sexp -> Reader [Table] Sexp
divideL = \case
  Int x := Int 0 := Nil -> return $ Atom "Please do not divideL by zero"
  Int x := Int y := Nil -> return $ Int (div x y)
  _ -> return $ Atom "Type Error: '/' takes two Ints"

negagtiveL :: Sexp -> Reader [Table] Sexp
negagtiveL = \case
  _ -> return $ Atom "a"

letL :: Sexp -> Reader [Table] Sexp
letL = \case
  Atom n := Atom "=" := x := Atom "in" := e -> do
    local ((n, Var x) :) $ eval e
  _ -> return $ Atom "Global def failure"

defunL :: Sexp -> Reader [Table] Sexp
defunL = \case
  Atom fn := xs := d -> return $ Atom "This should define a function"
  _ -> return $ Atom "Func Def Error"


arithmetic :: [Table]
arithmetic =
  [("+", Func plusL)
  ,("-", Func minusL)
  ,("*", Func multiplyL)
  ,("/", Func divideL)
  ]

initState :: [Table]
initState = ("defunL", Func defunL) : ("let", Func letL) : arithmetic

runEval :: Sexp -> Sexp
runEval = flip runReader state . eval where
  state = initState
