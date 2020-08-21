module Main where

import           Parse
import           Eval
import           Control.Monad                  ( when )

main :: IO ()
--main = do
  --_ <-
    --testEvalExp "( (num = (nil readInt nil)) in ( (((num % 3) == 0) && ((num % 5) == 0)) then ( (nil print \"FizzBuzz\") else ( ((num % 3) == 0) then ( (nil print \"Fizz\") else ( ((num % 5) == 0) then ( (nil print \"buzz\") else (nil print num))))))))"
  --pure ()
main = do
  inp <- getLine
  putStrLn ""
  result <- testEvalExp inp
  putStrLn $ ">> " ++ show result
  main



trustMe :: Either a b -> b
trustMe (Right x) = x
trustMe (Left  _) = error "YOU LIED TO ME"

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

