module Eval where


import           Parse
import qualified Data.List                     as L
import           Data.Maybe                     ( fromMaybe )
import           Data.Bifunctor                 ( bimap
                                                , second
                                                )


data Value = VPrim Primitive | VFunction Function deriving (Show)
data Primitive = VNum Int | VStr String | VNil deriving (Show)
newtype Function = Builtin (Value -> Value -> Value)

instance Show Function where
 show (Builtin _) = "builtin"

newtype Env = Env [(String , Value)] deriving (Show)






builtins :: [(String, Function)]
builtins = [("+", Builtin builtinPlus), ("-", Builtin builtinMinus)]
 where
  builtinPlus a1 a2 = VPrim $ case (a1, a2) of
   (VPrim (VNum a), VPrim (VNum b)) -> VNum $ a + b
   (VPrim (VStr a), VPrim (VStr b)) -> VStr $ a ++ b
   _ -> illegalFunctionArguments "+" [a1, a2]

  builtinMinus a1 a2 = VPrim $ case (a1, a2) of
   (VPrim (VNum a), VPrim (VNum b)) -> VNum $ a - b
   _ -> illegalFunctionArguments "+" [a1, a2]



illegalFunctionArguments :: String -> [Value] -> a
illegalFunctionArguments functionName args =
 error $ functionName ++ " called with illegal arguments " ++ show args


evalExp :: Env -> NExp -> Value
evalExp env expression = case expression of
 (ExpLit    lit   ) -> fromLiteral lit
 (ExpIdent  ident ) -> envLookup env (getName ident)
 (ExpLambda lambda) -> error "unimplemented"
 (ExpInvocation arg1 name arg2) ->
  let funcValue = envLookup env $ getName name
  in  case funcValue of
       VFunction (Builtin func) -> func (evalExp env arg1) (evalExp env arg2)
       _                        -> error $ getName name ++ "is not a function"



fromLiteral :: NLiteral -> Value
fromLiteral (StringLit s) = VPrim $ VStr s
fromLiteral (IntLit    n) = VPrim $ VNum n

envLookup :: Env -> String -> Value
envLookup (Env env) name = maybe (VPrim VNil) snd
 $ L.find (\(n, _) -> n == name) envWithBuiltins
 where envWithBuiltins = env ++ map (second VFunction) builtins

orNil :: Maybe Value -> Value
orNil = fromMaybe (VPrim VNil)


getName :: NIdent -> String
getName (Ident s) = s
