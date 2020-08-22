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
import qualified Data.Map.Strict               as M
import qualified Data.Either
import qualified Data.Text                     as T


runFunction :: Function -> Env -> Value -> Value -> IO Value
runFunction (Builtin f                             ) _   arg1 arg2 = f arg1 arg2
runFunction (Lambda lambdaEnv argName1 exp argName2) env arg1 arg2 = evalExp
  (envFromArgs <> lambdaEnv <> env)
  exp
 where
  envFromArgs = Env $ M.fromList $ case (argName1, argName2) of
    (Ident a     , Ident b     ) -> [(a, arg1), (b, arg2)]
    (IdentIgnored, Ident b     ) -> [(b, arg2)]
    (Ident a     , IdentIgnored) -> [(a, arg1)]
    _                            -> []


evalWithStdlib :: Env -> NExp -> IO Value
evalWithStdlib env exp = do
  stdEnv <- stdlib
  evalExp (env <> stdEnv) exp


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
    evalExp (bindings' <> env) blockExp

  ExpCondition yesExp condExp noExp -> do
    condResult <- evalExp env condExp
    case condResult of
      VBool True -> evalExp env yesExp
      VBool False -> evalExp env noExp
      _ -> error $ "condition " ++ show condResult ++ " is not a boolean"



evalBindings :: Env -> [(String, NExp)] -> IO Env
evalBindings _   []                 = pure mempty
evalBindings env ((name, exp) : xs) = do
  result <- evalExp env exp
  let newEnv = Env (M.singleton name result) <> env
  otherBindings <- evalBindings newEnv xs
  pure $ Env (M.singleton name result) <> otherBindings

fromLiteral :: NLiteral -> Value
fromLiteral lit = case lit of
  StringLit s -> VStr s
  IntLit    n -> VNum n
  BoolLit   b -> VBool b
  NilLit      -> VNil

envLookup :: Env -> String -> Value
envLookup (Env env) name = case M.lookup name envWithBuiltins of
  Just value -> value
  Nothing    -> error $ "Variable `" ++ name ++ "` is not in scope."
  where envWithBuiltins = env <> M.map VFunction builtins

getName :: NIdent -> String
getName (Ident s)    = s
getName IdentIgnored = error "tried to get the name of ignored identifier `_`"


stdlib :: IO Env
stdlib = evalBindings mempty $ Data.Either.rights $ map
  (getParsed . second parse)
  [ ( "map"
    , T.pack
      "[tuple ( { tail = (_ snd tuple) , } in ( (_ f (_ fst tuple)) , ((tail map f) <(tail != nil)> nil))) f]"
    )
  , ( ":"
    , T.pack
      "[lower (lower , (((lower + 1) : upper) <(lower < upper)> nil)) upper]"
    )
  ]


getParsed :: (a, Either e b) -> Either e (a, b)
getParsed (a, Right b) = Right (a, b)
getParsed (_, Left e ) = Left e

