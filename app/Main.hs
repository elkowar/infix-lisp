module Main where

import           Parse
import           Eval
import           Control.Monad                  ( when )
import qualified Text.Megaparsec.Error
import           Types

main :: IO ()
--main = do
  --_ <-
    --testEvalExp "( (num = (nil readInt nil)) in ( (nil print 'FizzBuzz') <(((num % 3) == 0) && ((num % 5) == 0))> ( (nil print 'Fizz') <((num % 3) == 0)> ( (nil print 'buzz') <((num % 5) == 0)> (nil print num)))))"
  --pure ()


main = do
  inp <- getContents
  --inp <- getLine
  putStrLn ""
  result <- testEvalExp inp
  putStrLn $ ">> " ++ show result
  pure ()
  --main



trustMe (Right x) = x
trustMe (Left  e) = error $ Text.Megaparsec.Error.errorBundlePretty e

testEvalExp :: String -> IO Value
testEvalExp s = evalExp (Env []) (trustMe $ parse s)


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

