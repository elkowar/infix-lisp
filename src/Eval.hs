module Eval where


import           Parse
import qualified Data.List                     as L
import           Data.Maybe                     ( fromMaybe )


newtype Value = VPrimitive Primitive deriving (Show)
data Primitive = VNumber Int | VString String | VNil deriving (Show)

newtype Env = Env [(String , Value)] deriving (Show)




evalExp :: Env -> NExp -> String
evalExp _ (ExpLit lit) = show lit
evalExp (Env env) (ExpIdent ident) =
  show . orNil $ snd <$> L.find (\(n, _) -> n == getName ident) env
evalExp (Env env) (ExpLambda lambda            ) = show lambda
evalExp (Env env) (ExpInvocation arg1 name arg2) = error "HI"



orNil :: Maybe Value -> Value
orNil = fromMaybe (VPrimitive VNil)


getName :: NIdent -> String
getName (Ident s) = s
