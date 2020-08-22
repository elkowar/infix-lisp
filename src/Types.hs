{-# OPTIONS_GHC -Wall -fno-warn-unused-imports -fno-warn-missing-signatures -fno-warn-missing-pattern-synonym-signatures #-}
{-# LANGUAGE GeneralizedNewtypeDeriving, PatternSynonyms #-}
module Types where

import Parse
import qualified Data.Map.Strict as M


data Value = VNum !Int
           | VStr !String
           | VBool !Bool
           | VNil
           | VFunction !Function
           | VTuple !Value !Value
           deriving (Eq)
data Function = Builtin (Value -> Value -> IO Value) | Lambda Env !NIdent !NExp !NIdent

instance Eq Function where
  (==) (Lambda _ a1 exp1 b1) (Lambda _ a2 exp2 b2) = (a1, exp1, b1) == (a2, exp2, b2)
  (==) _ _ = False

pattern VLambda a b c d = VFunction (Lambda a b c d)
pattern VBuiltin f = VFunction (Builtin f)

instance Show Value where
  show (VFunction f) = show f
  show (VTuple a b) = show (a, b)
  show (VNum n) = show n
  show (VStr s) = s
  show (VBool b) = show b
  show VNil = "nil"


instance Show Function where
  show (Builtin _        ) = "builtin"
  show (Lambda _ a1 body a2) = unwords ["[", show a1, show body, show a2, "]"]

newtype Env = Env (M.Map String Value) deriving (Show, Semigroup)

instance Monoid Env where
  mappend (Env a) (Env b) = Env $ M.union a b
  mempty = Env mempty
