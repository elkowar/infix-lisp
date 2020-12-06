{-# OPTIONS_GHC -Wall -fno-warn-unused-imports -fno-warn-missing-signatures -fno-warn-missing-pattern-synonym-signatures #-}
module Builtins
  ( builtins
  )
where
import           Types
import qualified Data.Map.Strict               as M
import qualified Parse


builtins :: M.Map String Function
builtins = M.fromList
  [ ("+"         , Builtin builtinPlus)
  , ("-"         , Builtin builtinMinus)
  , ("*"         , Builtin builtinTimes)
  , ("/"         , Builtin builtinDiv)
  , ("%"         , Builtin builtinMod)
  , ("<"         , Builtin builtinLT)
  , (">"         , Builtin builtinGT)
  , ("&&"        , Builtin builtinAnd)
  , ("||"        , Builtin builtinOr)
  , ("=="        , Builtin builtinEq)
  , ("!="        , Builtin builtinNeq)
  , (","         , Builtin builtinMakeTuple)
  , ("fst"       , Builtin builtinFirst)
  , ("snd"       , Builtin builtinSecond)
  , ("head"      , Builtin builtinFirst)
  , ("tail"      , Builtin builtinSecond)
  , ("toCharList", Builtin builtinToCharList)
  , ("print"     , Builtin builtinPrint)
  , ("readString", Builtin builtinReadString)
  , ("readFile"  , Builtin builtinReadFile)
  , ("readInt"   , Builtin builtinReadInt)
  , ("toString"  , Builtin builtinToString)
  ]
 where
  builtinPlus (VNum a) (VNum b) = pure . VNum $ a + b
  builtinPlus (VStr a) (VStr b) = pure . VStr $ a ++ b
  builtinPlus a        b        = illegalFunctionArguments "+" [a, b]

  builtinMinus = numFunc "-" $ \a b -> VNum $ a - b
  builtinTimes = numFunc "*" $ \a b -> VNum $ a * b
  builtinMod   = numFunc "%" $ \a b -> VNum $ a `mod` b
  builtinDiv   = numFunc "/" $ \a b -> VNum $ a `div` b
  builtinLT    = numFunc "<" $ \a b -> VBool $ a < b
  builtinGT    = numFunc ">" $ \a b -> VBool $ a > b

  builtinAnd   = boolFunc "&&" (&&)
  builtinOr    = boolFunc "||" (||)

  builtinEq a b = pure . VBool $ a == b
  builtinNeq a b = pure . VBool $ a /= b

  builtinPrint _ value =
    let str = case value of
          VStr s -> s
          VNum n -> show n
          VNil   -> "nil"
          x      -> show x
    in  putStrLn str >> pure VNil

  builtinReadString _ _ = VStr <$> getLine
  builtinReadInt _ _ = VNum <$> readLn

  builtinReadFile _ (VStr path) = VStr <$> readFile path
  builtinReadFile a b = illegalFunctionArguments "readFile" [a, b]

  builtinMakeTuple a b = pure $ VTuple a b

  builtinFirst _ (VTuple a _) = pure a
  builtinFirst _ (VStr (x:_)) = pure $ VStr [x]
  builtinFirst a b            = illegalFunctionArguments "fst" [a, b]

  builtinSecond _ (VTuple _ b) = pure b
  builtinSecond _ (VStr (_:xs)) = pure $ VStr xs
  builtinSecond a b            = illegalFunctionArguments "snd" [a, b]
  
  builtinToString _ value = pure . VStr $ show value

  builtinToCharList :: Value -> Value -> IO Value
  builtinToCharList _ (VStr str) = case str of
    (x : xs) -> VTuple (VStr [x]) <$> builtinToCharList VNil (VStr xs)
    []       -> pure VNil
  builtinToCharList a b = illegalFunctionArguments "toCharList" [a, b]

  boolFunc :: String -> (Bool -> Bool -> Bool) -> Value -> Value -> IO Value
  boolFunc _    f (VBool a) (VBool b) = pure . VBool $ f a b
  boolFunc name _ a         b         = illegalFunctionArguments name [a, b]

  numFunc :: String -> (Int -> Int -> Value) -> Value -> Value -> IO Value
  numFunc _    f (VNum a) (VNum b) = pure $ f a b
  numFunc name _ a        b        = illegalFunctionArguments name [a, b]




illegalFunctionArguments :: String -> [Value] -> a
illegalFunctionArguments functionName args =
  error $ functionName ++ " called with illegal arguments " ++ show args

