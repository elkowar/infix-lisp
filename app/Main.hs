{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import           Parse
import           Eval
import           Control.Monad                  ( when )
import           Control.Monad.IO.Class         ( liftIO )
import           Control.Exception              ( catch )
import qualified Text.Megaparsec.Error
import           Types
import qualified Data.Text                     as T
import           System.Console.Repline
import           System.Environment

type Repl a = HaskelineT IO a

cmd :: String -> Repl ()
cmd input = dontCrash $ do
  result <- liftIO $ testEvalExp input
  liftIO $ putStrLn $ ">> " ++ show result
  pure ()


ini :: Repl ()
ini =
  liftIO $ putStrLn "Have fun in one of the dumbest languages in existance!"

completer :: Monad m => WordCompleter m
completer word = pure []

repl :: IO ()
repl = evalRepl (pure ">>> ") cmd [] Nothing (Word completer) ini

main :: IO ()
main = do
  args <- getArgs
  if null args then repl else runFile (head args)

runFile :: String -> IO ()
runFile path = do
  code   <- readFile path
  result <- testEvalExp code
  pure ()


trustMe (Right x) = x
trustMe (Left  e) = error $ Text.Megaparsec.Error.errorBundlePretty e

testEvalExp :: String -> IO Value
testEvalExp s = evalWithStdlib mempty (trustMe $ parse (T.pack s))


--(
  --(num = (nil readInt nil)) 
--in 
  --(
    --(((num % 3) == 0) && ((num % 5) == 0))
  --then 
    --(
      --(nil print "FizzBuzz")
    --else 
      --(
        --((num % 3) == 0)
      --then 
        --(
          --(nil print "Fizz")
        --else
          --(
            --((num % 5) == 0)
          --then
            --(
              --(nil print "buzz")
            --else 
              --(nil print num)
            --)
          --)
        --)
      --)
    --)
  --)
--)

