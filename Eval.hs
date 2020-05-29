{-# LANGUAGE LambdaCase #-}

module Eval where

import Control.Monad.Except
import Parser (Sexp (..), Table, State, Error)


raise :: String -> Either Error a
raise = throwError

table :: Either Error State -> Either Error Table
table = fmap fst

sexp :: Either Error State -> Either Error Sexp
sexp = fmap snd

map' :: Monad m => (Sexp -> m Sexp) -> Sexp -> m Sexp
map' f (x := y) = (:=) <$> f x <*> map' f y
map' f x = f x

eval :: State -> Either Error State
eval (t, e) = case e of
  FuncL f := x -> f (t, x)
  AtomL a -> do
    a' <- getBind (t, AtomL a)
    eval (t, a')
  x := ys -> do
    (_, x') <- eval (t, x)
    eval (t, x' := ys)
  x -> Right (t, x)

getBind :: State -> Either Error Sexp
getBind = \case
  ([], _) -> raise "Error: exhasted bindings"
  ((n, f) : ts, AtomL s)
    | n == s -> Right f
    | otherwise -> getBind (ts, AtomL s)

evalList :: State -> Either Error State
evalList (t, xs) = do
  xs' <- map' (\x -> sexp $ eval (t, x)) xs
  return (t, xs')


-- Arithmetic --

-- sums a list of IntLs
plusL :: State -> Either Error State
plusL xs = do
  xs' <- evalList xs
  case xs' of
    (t, IntL n := NilL) -> return (t, IntL n)
    (t, IntL n1 := IntL n2) -> return (t, IntL (n1 + n2))
    (t, IntL n := ns) -> do
      (_, ns') <- plusL (t, ns)
      plusL (t, IntL n := ns')
    (_, z) -> raise $ "Type Error: '+' takes Ints,'n  " ++ show z ++ " do not have type Int"

-- subtracts two IntLs
minusL :: State -> Either Error State
minusL xs = do
  xs' <- evalList xs
  case xs' of
    (t, IntL n1 := IntL n2 := NilL) -> return (t, IntL (n1 - n2))
    (_, z) -> raise $ "Type Error: '-' takes two Ints, \n  " ++ show z ++ " do not have type Int"

-- multiplies two IntLs
multiplyL :: State -> Either Error State
multiplyL xs = do
  xs' <- evalList xs
  case xs' of
    (t, IntL n := NilL) -> return (t, IntL n)
    (t, IntL n1 := IntL n2) -> return (t, IntL (n1 * n2))
    (t, IntL n := ns) -> do
      (_, ns') <- multiplyL (t, ns)
      multiplyL (t, IntL n := ns')
    (_, z) -> raise $ "Type Error: '*' takes IntLs,\n  "++ show z ++ " do not have type Int"

-- divides two IntLs
divideL :: State -> Either Error State
divideL xs = do
  xs' <- evalList xs
  case xs' of
    (_, IntL _ := IntL 0 := NilL) -> raise "Please do not divide by zero"
    (t, IntL n1 := IntL n2 := NilL) -> return (t, IntL (div n1 n2))
    (_, z) -> raise $ "Type Error: '/' takes two IntLs,\n  " ++ show z ++ " do not have type Int"

-- negates one IntL
negativeL :: State -> Either Error State
negativeL xs = do
  xs' <- eval xs
  case xs' of
    (t', IntL n) -> return (t', IntL (- n))
    (_, z) -> raise $ "Type Error: 'neg' takes one IntL\n  " ++ show z ++ " does not have type Int"

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
letL :: State -> Either Error State
letL = \case
  (t, AtomL n :=  x := e := NilL) -> Right (t', e')
    where
    t' = (n, x) : t
    Right (_, e') = eval (t', e)
  _ -> raise "Syntax Error: Invalid let expression"

extend :: Sexp -> Sexp -> Table -> Maybe Table
extend NilL NilL t = Just t
extend NilL vs _ = Nothing
extend xs NilL _ = Nothing
extend (AtomL x := xs) (v := vs) t = ((x, v) :) <$> extend xs vs t

-- defines a lambda expression
lambdaL :: State -> Either Error State
lambdaL = \case
  (t, xs := ds := NilL) -> eval (t, FuncL fn)
    where
      fn (t', es) = let Right (_, vs) = evalList (t', es) in
        case extend xs vs t of
          Just t -> eval (t, ds)
          Nothing -> raise "Error: incorrect number of args in lambda expression"
  (t, _) -> raise "Syntax Error: invalid lambda expression"

-- defines a recursive lambda expression
fixL :: State -> Either Error State
fixL = \case
  (t, AtomL f := xs := d := NilL) -> eval (t, FuncL fn)
    where
    fn (t', es) = let Right (_, vs) = evalList (t', es) in
      case extend xs vs ((f, FuncL fn) : t) of
        Just t -> eval (t, d)
        Nothing -> raise "Error: incorrect number of args in fix expression"
  _ -> raise "Error: invalid fix expression"

lets :: Table
lets =
  [ ("let", FuncL letL)
  , ("lambda", FuncL lambdaL)
  , ("fix", FuncL fixL)
  ]


-- Bool --

equalsL :: State -> Either Error State
equalsL (t, x) = case x of
  _ := NilL -> raise "Type Error: '=' takes two arguments"
  a := b := NilL -> return (t, BoolL $ a' == b')
    where
    a' = sexp $ eval (t, a)
    b' = sexp $ eval (t, b)

andL :: State -> Either Error State
andL (t, x) = do
  (_, x') <- eval (t, x)
  case stripNilL x' of
    BoolL True := BoolL True -> return (t, BoolL True)
    BoolL _ := BoolL _ -> return (t, BoolL False)
    _ -> raise "Type Error: 'and' takes two Bools"

orL :: State -> Either Error State
orL (t, x) = do
  (_, x') <- eval (t, x)
  case stripNilL x' of
    BoolL True := BoolL _ -> return (t, BoolL True)
    BoolL _ := BoolL True -> return (t, BoolL True)
    BoolL False := BoolL False -> return (t, BoolL False)
    _ -> raise "Type Error: 'or' takes two Bools"

xorL :: State -> Either Error State
xorL (t, x) = do
  (_, x') <- eval (t, x)
  case stripNilL x' of
    BoolL True := BoolL False -> return (t, BoolL True)
    BoolL False := BoolL True -> return (t, BoolL True)
    BoolL _ := BoolL _ -> return (t, BoolL False)
    _ -> raise "Type Error: 'xor' takes two Bools"

notL :: State -> Either Error State
notL (t, x) = do
  (_, x') <- eval (t, x)
  case stripNilL x' of
    BoolL True -> return (t, BoolL False)
    BoolL False -> return (t, BoolL True)
    _ -> raise "Type Error: 'not' takes one Bool"

ifL :: State -> Either Error State
ifL (t, p := d := e := NilL) = do
  (_, p') <- eval (t, p)
  case stripNilL p' of
    BoolL True -> eval (t, d)
    BoolL False -> eval (t, e)
    _ -> raise "Type Error: 'if' takes one Bool and two expressions"

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

runEval :: Sexp -> Either Error Sexp
runEval x = do
  (_, x') <- eval (builtins, x)
  return $ stripNilL x'
