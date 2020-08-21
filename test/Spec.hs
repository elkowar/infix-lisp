
import           Parse
import           Eval



main :: IO ()
main = do
  testParser
  --_ <- testEval
  pure ()


--testEval = do
  --testEvalExp [("a", VPrim (VNum 12))] "a"
  --testEvalExp []                       "\"a\""
  --testEvalExp []                       "(1 + 5)"
  --testEvalExp []                       "((1 + 2) + 5)"
  --testEvalExp []                       "(nil print \"hi\")"
  --testEvalExp []                       "(nil print [x (x + 1) y])"




trustMe :: Either a b -> b
trustMe (Right x) = x
trustMe (Left  _) = error "YOU LIED TO ME"

--testEvalExp env s = do
  --result <- evalExp (Env env) $ trustMe $ parse s
  --print result



testParser = do
  --parseTest "12"
  --parseTest "(12 identifier 12)"
  --parseTest "((12 ident 15) otherIdent 0)"
  --parseTest "((\"Muhh string\" ident 15) otherIdent 0)"
  --parseTest "[arg1 (arg1 + arg2) arg2]"
  --parseTest "(addNums = [arg1 (arg1 plus arg2) arg2])"
  parseTest "(nil print [x (x + 1) y])"
  parseTest "(nil [_ (x + 1) x] 5)"
  --parseTest "[12 (\"hoiiii\") foo]"
