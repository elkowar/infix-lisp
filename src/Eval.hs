{-# OPTIONS_GHC -Wall -fno-warn-unused-imports -fno-warn-missing-signatures -fno-warn-missing-pattern-synonym-signatures #-}
{-# LANGUAGE LambdaCase, PatternSynonyms #-}
module Eval where

import           Prelude                 hiding ( exp )
import           Parse
import qualified Data.List                     as L
import           Data.Maybe                     ( fromMaybe )
import           Data.Bifunctor                 ( bimap
                                                , first
                                                , second
                                                )
import           Builtins
import           Types


runFunction :: Function -> Env -> Value -> Value -> IO Value
runFunction (Builtin f                             ) _   arg1 arg2 = f arg1 arg2
runFunction (Lambda lambdaEnv argName1 exp argName2) env arg1 arg2 = evalExp
  (envFromArgs <> lambdaEnv <> env)
  exp
 where
  envFromArgs = Env $ case (argName1, argName2) of
    (Ident a     , Ident b     ) -> [(a, arg1), (b, arg2)]
    (IdentIgnored, Ident b     ) -> [(b, arg2)]
    (Ident a     , IdentIgnored) -> [(a, arg1)]
    _                            -> []



evalExp :: Env -> NExp -> IO Value
evalExp env expression = case expression of
  ExpLit   lit                 -> pure $ fromLiteral lit

  ExpIdent (Ident ident)       -> pure $ envLookup env ident
  ExpIdent IdentIgnored        -> error "Tried to reference ignored identifier"

  ExpLambda     arg1 body arg2 -> pure $ VFunction $ Lambda env arg1 body arg2

  ExpInvocation arg1 exp  arg2 -> evalExp env exp >>= \case
    VFunction func -> do
      res1 <- evalExp env arg1
      res2 <- evalExp env arg2
      runFunction func env res1 res2
    wannabeFunction -> error $ show wannabeFunction ++ " is not a function"

  ExpInBinding bindings blockExp -> do
    bindings' <- evalBindings env (fmap (first getName) bindings)
    evalExp (Env bindings' <> env) blockExp

  ExpCondition yesExp condExp noExp -> do
    condResult <- evalExp env condExp
    case condResult of
      VBool True -> evalExp env yesExp
      VBool False -> evalExp env noExp
      _ -> error $ "condition " ++ show condResult ++ " is not a boolean"



evalBindings :: Env -> [(String, NExp)] -> IO [(String, Value)]
evalBindings _   []                 = pure []
evalBindings env ((name, exp) : xs) = do
  result <- evalExp env exp
  let newBinding = (name, result)
  otherBindings <- evalBindings (env <> Env [newBinding]) xs
  pure $ newBinding : otherBindings

fromLiteral :: NLiteral -> Value
fromLiteral lit = case lit of
  StringLit s -> VStr s
  IntLit    n -> VNum n
  BoolLit   b -> VBool b
  NilLit      -> VNil

envLookup :: Env -> String -> Value
envLookup (Env env) name = case result of
  Just value -> snd value
  Nothing    -> error $ "Variable `" ++ name ++ "` is not in scope."
 where
  envWithBuiltins = env ++ map (second VFunction) builtins
  result          = L.find (\(n, _) -> n == name) envWithBuiltins

getName :: NIdent -> String
getName (Ident s)    = s
getName IdentIgnored = error "tried to get the name of ignored identifier `_`"
