
import           Parse
import           Eval



main :: IO ()
main = do
  --testParser
  _ <- testEval
  pure ()


testEval = do
  testEvalExp "\"a\""
  testEvalExp "(1 + 5)"
  testEvalExp "((1 + 2) + 5)"
  --testEvalExp "(nil print \"hi\")"
  --testEvalExp "(nil print [x (x + 1) y])"
  testEvalExp "(( x = 1 ) in (x + 2))"
  testEvalExp "(( add = [a (a + b) b] ) in (2 add 5))"
  testEvalExp "((1 == 1) then (\"yes\" else \"no\"))"
  testEvalExp
    "((not = [_ (value then (false else true)) value]) in (_ not true))"
  testEvalExp
    "((not = [_ (value then (false else true)) value]) in (_ not false))"
  testEvalExp "(1 [a (a + b) b] 1)"
  testEvalExp "((add4 = [a [b ((a + b) + (c + d)) c] d]) in (1 (1 add4 1) 1))"




trustMe :: Either a b -> b
trustMe (Right x) = x
trustMe (Left  _) = error "YOU LIED TO ME"

testEvalExp :: String -> IO ()
testEvalExp s = do
  result <- evalExp (Env []) (trustMe $ parse s)
  putStrLn $ s ++ " ===> " ++ show result



testParser = do
  --parseTest "[a a b]"
  --parseTest "(1 [ a ( a + b ) b ] 2)"
  --parseTest "12"
  --parseTest "(12 identifier 12)"
  --parseTest "((12 ident 15) otherIdent 0)"
  --parseTest "((\"Muhh string\" ident 15) otherIdent 0)"
  --parseTest "[arg1 (arg1 + arg2) arg2]"
  --parseTest "(addNums = [arg1 (arg1 plus arg2) arg2])"
  --parseTest "( nil print [ x ( x + 1 ) y ] )"
  --parseTest "( nil [ _ ( x + 1 ) x ] 5 )"
  --parseTest "( ( x = 1 ) in x)"
  --parseTest "(1 then (12 else 0))"
  parseTest "((add4 = [a [b ((a + b) + (c + d)) c] d]) in (1 (1 add4 1) 1))"
