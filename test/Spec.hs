
import           Parse
import           Eval
import           Types
import qualified Data.Text                     as T



main :: IO ()
main = do
  testParser
  _ <- testEval
  pure ()


testEval = do
  testEvalExp "\"a\""
  testEvalExp "(1 + 5)"
  testEvalExp "((1 + 2) + 5)"
  testEvalExp "({ x = 1, } in (x + 2))"
  testEvalExp "({ add = [a (a + b) b], } in (2 add 5))"
  testEvalExp "(\"yes\" <(1 == 1)> \"no\")"
  testEvalExp "({not = [_ (false <value> true) value], } in (nil not true))"
  testEvalExp "({not = [_ (false <value> true) value], } in (nil not false))"
  testEvalExp "(1 [a (a + b) b] 1)"
  testEvalExp "({add4 = [a [b ((a + b) + (c + d)) c] d], } in (1 (1 add4 1) 1))"





trustMe :: Either a b -> b
trustMe (Right x) = x
trustMe (Left  _) = error "YOU LIED TO ME"

testEvalExp :: String -> IO ()
testEvalExp s = do
  result <- evalExp mempty (trustMe $ parse $ T.pack s)
  putStrLn $ s ++ " ===> " ++ show result


testParser = do
  parseTest "({ a = 12, b = 13, c = 14 } in a)"
  parseTest "[a a b]"
  parseTest "(1 [ a ( a + b ) b ] 2)"
  parseTest "12"
  parseTest "(12 identifier 12)"
  parseTest "((12 ident 15) otherIdent 0)"
  parseTest "((\"Muhh string\" ident 15) otherIdent 0)"
  parseTest "[arg1 (arg1 + arg2) arg2]"
  parseTest "( nil print [ x ( x + 1 ) y ] )"
  parseTest "( nil [ _ ( x + 1 ) x ] 5 )"
  parseTest "( { x = 1, } in x)"
  parseTest "(1 <(true && false)> 2)"
  parseTest "({ add4 = [a [b ((a + b) + (c + d)) c] d] } in (1 (1 add4 1) 1))"
