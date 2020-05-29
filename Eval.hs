{-# LANGUAGE LambdaCase #-}

module Eval where

import Parser (Sexp (..), Table, State, Error)


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
  AtomL a -> case getBind (t, AtomL a) of
    Left e -> Left e
    Right x -> eval (t, x)
  x := ys -> do
    (_, x') <- eval (t, x)
    eval (t, x' := ys)
  x -> Right (t, x)

getBind :: State -> Either Error Sexp
getBind = \case
  ([], _) -> Left "Error: exhasted bindings"
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
plusL x = case evalList x of
  Left e -> Left e
  Right (t, IntL n1 := IntL n2) -> Right (t, IntL (n1 + n2))
  Right (t, IntL n := NilL) -> Right (t, IntL n)
  Right (t, IntL n := ns) -> case plusL (t, ns) of
    Left e -> Left e
    Right (_, n1) -> plusL (t, IntL n := n1)
  Right (_, z) -> Left $ "Type Error: '+' takes Ints,\n  " ++ show z ++ " do not have type Int"

-- subtracts two IntLs
minusL :: State -> Either Error State
minusL x = case evalList x of
  Left e -> Left e
  Right (t, IntL n1 := IntL n2 := NilL) -> Right (t, IntL (n1 - n2))
  Right (_, z) -> Left $ "Type Error: '-' takes two Ints,\n  " ++ show z ++ " do not have type Int"

-- multiplies two IntLs
multiplyL :: State -> Either Error State
multiplyL x = case evalList x of
  Left e -> Left e
  Right (t, IntL n1 := IntL n2) -> Right (t, IntL (n1 * n2))
  Right (t, IntL n := NilL) -> Right (t, IntL n)
  Right (t, IntL n1 := n2) -> case multiplyL (t, n2) of
    Left e -> Left e
    Right (_, n') -> multiplyL (t, IntL n1 := n')
  Right (_, z) -> Left $ "Type Error: '*' takes IntLs,\n  "++ show z ++ " do not have type Int"

-- divides two IntLs
divideL :: State -> Either Error State
divideL x = case evalList x of
  Left e -> Left e
  Right (_, IntL _ := IntL 0 := NilL) -> Left "Please do not divide by zero"
  Right (t', IntL n1 := IntL n2 := NilL) -> Right (t', IntL (div n1 n2))
  Right (_, z) -> Left $ "Type Error: '/' takes two IntLs,\n  " ++ show z ++ " do not have type Int"

-- negates one IntL
negativeL :: State -> Either Error State
negativeL x = case eval x of
  Left e -> Left e
  Right (t', IntL n) -> Right (t', IntL (- n))
  Right (_, z) -> Left $ "Type Error: 'neg' takes one IntL\n  " ++ show z ++ " does not have type Int"

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
  _ -> Left "Syntax Error: Invalid let expression"

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
          Nothing -> Left "Error: incorrect number of args in lambda expression"
  (t, _) -> Left "Syntax Error: invalid lambda expression"

-- defines a recursive lambda expression
fixL :: State -> Either Error State
fixL = \case
  (t, AtomL f := xs := d := NilL) -> eval (t, FuncL fn)
    where
    fn (t', es) = let Right (_, vs) = evalList (t', es) in
      case extend xs vs ((f, FuncL fn) : t) of
        Just t -> eval (t, d)
        Nothing -> Left "Error: incorrect number of args in fix expression"
  _ -> Left "Error: invalid fix expression"

lets :: Table
lets =
  [ ("let", FuncL letL)
  , ("lambda", FuncL lambdaL)
  , ("fix", FuncL fixL)
  ]


-- Bool --

equalsL :: State -> Either Error State
equalsL (t, x) = case x of
  _ := NilL -> Left "Type Error: '=' takes two arguments"
  a := b := NilL -> Right (t, BoolL $ a' == b')
    where
    a' = sexp $ eval (t, a)
    b' = sexp $ eval (t, b)

andL :: State -> Either Error State
andL (t, x) = do
  (_, x') <- eval (t, x)
  case stripNilL x' of
    BoolL True := BoolL True -> Right (t, BoolL True)
    BoolL _ := BoolL _ -> Right (t, BoolL False)
    _ -> Left "Type Error: 'and' takes two Bools"

orL :: State -> Either Error State
orL (t, x) = do
  (_, x') <- eval (t, x)
  case stripNilL x' of
    BoolL True := BoolL _ -> Right (t, BoolL True)
    BoolL _ := BoolL True -> Right (t, BoolL True)
    BoolL False := BoolL False -> Right (t, BoolL False)
    _ -> Left "Type Error: 'or' takes two Bools"

xorL :: State -> Either Error State
xorL (t, x) = do
  (_, x') <- eval (t, x)
  case stripNilL x' of
    BoolL True := BoolL False -> Right (t, BoolL True)
    BoolL False := BoolL True -> Right (t, BoolL True)
    BoolL _ := BoolL _ -> Right (t, BoolL False)
    _ -> Left "Type Error: 'xor' takes two Bools"

notL :: State -> Either Error State
notL (t, x) = do
  (_, x') <- eval (t, x)
  case stripNilL x' of
    BoolL True -> Right (t, BoolL False)
    BoolL False -> Right (t, BoolL True)
    _ -> Left "Type Error: 'not' takes one Bool"

ifL :: State -> Either Error State
ifL (t, p := d := e := NilL) = do
  (_, p') <- eval (t, p)
  case stripNilL p' of
    BoolL True -> eval (t, d)
    BoolL False -> eval (t, e)
    _ -> Left "Type Error: 'if' takes one Bool and two expressions"

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
  Right $ stripNilL x'
