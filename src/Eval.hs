{-# LANGUAGE GeneralizedNewtypeDeriving, LambdaCase, PatternSynonyms #-}
module Eval where


import           Parse
import qualified Data.List                     as L
import           Data.Maybe                     ( fromMaybe )
import           Data.Bifunctor                 ( bimap
                                                , first
                                                , second
                                                )


data Value = VPrim Primitive | VFunction Function | VTuple Value Value deriving (Eq)
data Primitive = VNum Int | VStr String | VBool Bool | VNil deriving (Eq)
data Function = Builtin (Value -> Value -> IO Value) | Lambda Env String NExp String

instance Eq Function where
  (==) (Lambda _ a1 exp1 b1) (Lambda _ a2 exp2 b2) = (a2, exp2, b2) == (a2, exp2, b2)
  (==) _ _ = False

pattern PatNum a = VPrim (VNum a)
pattern PatStr a = VPrim (VStr a)
pattern PatBool a = VPrim (VBool a)
pattern PatLambda a b c d = VFunction (Lambda a b c d)
pattern PatBuiltin f = VFunction (Builtin f)

instance Show Value where
  show (VPrim p) = show p
  show (VFunction f) = show f
  show (VTuple a b) = show (a, b)

instance Show Primitive where
  show (VNum n) = if n == 69 then "69,... nice!" else show n
  show (VStr s) = s
  show (VBool b) = show b
  show VNil = "nil"

instance Show Function where
  show (Builtin _        ) = "builtin"
  show (Lambda env a1 body a2) = unwords ["[", a1, show body, a2, "]"]

newtype Env = Env [(String , Value)] deriving (Show, Semigroup, Monoid)



runFunction :: Function -> Env -> Value -> Value -> IO Value
runFunction (Builtin f) _ arg1 arg2 = f arg1 arg2
runFunction (Lambda lambdaEnv argName1 exp argName2) env arg1 arg2 =
  evalExp (Env [(argName1, arg1), (argName2, arg2)] <> lambdaEnv <> env) exp



builtins :: [(String, Function)]
builtins =
  [ ("+", Builtin builtinPlus)
  , ("-", Builtin builtinMinus)
  , ("*", Builtin builtinTimes)
  , ("/", Builtin builtinDiv)
  , ("%", Builtin builtinMod)
  , ("<", Builtin builtinLT)
  , (">", Builtin builtinGT)
  , ("&&", Builtin builtinAnd)
  , ("||", Builtin builtinOr)
  , ("==", Builtin builtinEq)
  , ("!=", Builtin builtinNeq)
  , (",", Builtin builtinMakeTuple)
  , ("fst", Builtin builtinTupleFirst)
  , ("snd", Builtin builtinTupleSecond)
  , ("print", Builtin builtinPrint)
  , ("readString", Builtin builtinReadString)
  , ("readInt", Builtin builtinReadInt)
  ]
 where
  builtinPlus (PatNum a) (PatNum b) = pure . PatNum $ a + b
  builtinPlus (PatStr a) (PatStr b) = pure . PatStr $ a ++ b
  builtinPlus a b = illegalFunctionArguments "+" [a, b]

  builtinMinus = numFunc "-" $ \a b -> PatNum $ a - b
  builtinTimes = numFunc "*" $ \a b -> PatNum $ a * b
  builtinMod   = numFunc "%" $ \a b -> PatNum $ a `mod` b
  builtinDiv   = numFunc "/" $ \a b -> PatNum $ a `div` b
  builtinLT    = numFunc "<" $ \a b -> PatBool $ a < b
  builtinGT    = numFunc ">" $ \a b -> PatBool $ a > b

  builtinAnd = boolFunc "&&" (&&)
  builtinOr  = boolFunc "||" (||)

  builtinEq a b = pure . PatBool $ a == b
  builtinNeq a b = pure . PatBool $ a /= b

  builtinPrint _ value = let str = case value of PatStr s -> s
                                                 PatNum n -> show n
                                                 VPrim VNil -> "nil"
                                                 x -> show x
                          in putStrLn str >> pure (VPrim VNil)

  builtinReadString _ _ = PatStr <$> getLine
  builtinReadInt _ _ = PatNum <$> readLn

  builtinMakeTuple a b = pure $ VTuple a b

  builtinTupleFirst _ (VTuple a b) = pure a
  builtinTupleFirst a b = illegalFunctionArguments "fst" [a, b]

  builtinTupleSecond _ (VTuple a b) = pure b
  builtinTupleSecond a b = illegalFunctionArguments "snd" [a, b]

  boolFunc :: String -> (Bool -> Bool -> Bool) -> Value -> Value -> IO Value
  boolFunc name f (PatBool a) (PatBool b) = pure . PatBool $ f a b
  boolFunc name f a b = illegalFunctionArguments name [a, b]

  numFunc :: String -> (Int -> Int -> Value) -> Value -> Value -> IO Value
  numFunc name f (PatNum a) (PatNum b) = pure $ f a b
  numFunc name f a b = illegalFunctionArguments name [a, b]






illegalFunctionArguments :: String -> [Value] -> a
illegalFunctionArguments functionName args =
  error $ functionName ++ " called with illegal arguments " ++ show args


evalExp :: Env -> NExp -> IO Value
evalExp env expression = case expression of
  ExpLit   lit -> pure $ fromLiteral lit

  ExpIdent (Ident ident) -> pure $ envLookup env ident

  ExpLambda (Ident argName1) body (Ident argName2) ->
    pure $ VFunction $ Lambda env argName1 body argName2

  ExpInvocation arg1 exp arg2 -> evalExp env exp >>= \case
    VFunction func -> do
      res1 <- evalExp env arg1
      res2 <- evalExp env arg2
      runFunction func env res1 res2
    wannabeFunction -> error $ show wannabeFunction ++ " is not a function"

  ExpInBinding bindings blockExp -> do
    bindings <- evalBindings env (fmap (first getName) bindings)
    evalExp (Env bindings <> env) blockExp

  ExpCondition yesExp condExp noExp -> do
    condResult <- evalExp env condExp
    case condResult of
      PatBool True -> evalExp env yesExp
      PatBool False -> evalExp env noExp
      _ -> error $ "condition " ++ show condResult ++ " is not a boolean"



evalBindings :: Env -> [(String, NExp)] -> IO [(String, Value)]
evalBindings _ [] = pure []
evalBindings env ((name, exp):xs) = do
  result <- evalExp env exp
  let newBinding = (name, result)
  otherBindings <- evalBindings (env <> Env [newBinding]) xs
  pure $ newBinding : otherBindings

fromLiteral :: NLiteral -> Value
fromLiteral lit = case lit of
  StringLit s -> VPrim $ VStr s
  IntLit    n -> VPrim $ VNum n
  BoolLit   b -> VPrim $ VBool b
  NilLit      -> VPrim VNil

envLookup :: Env -> String -> Value
envLoopup _ "_" = error "Tried to look for variable '_' in environment, but '_' is not a valid identifier"
envLookup (Env env) name = case result of
    Just value -> snd value
    Nothing -> error $ "Variable " ++ name ++ " is not in scope."
  where
    envWithBuiltins = env ++ map (second VFunction) builtins
    result = L.find (\(n, _) -> n == name) envWithBuiltins

orNil :: Maybe Value -> Value
orNil = fromMaybe (VPrim VNil)


getName :: NIdent -> String
getName (Ident s) = s
