module Main where

import           Parse
import           Eval

main :: IO ()
main = do
  testEvalExp "1"



trustMe :: Either a b -> b
trustMe (Right x) = x
trustMe (Left  _) = error "YOU LIED TO ME"

testEvalExp :: String -> IO ()
testEvalExp s = print =<< evalExp (Env []) (trustMe $ parse s)
