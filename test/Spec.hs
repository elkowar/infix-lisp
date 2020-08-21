
import           Parse
import           Eval



main :: IO ()
main = do
  testEval
  --testParser


testEval = do
  testEvalExp [("a", VPrim (VNum 12))] "a"
  testEvalExp []                       "\"a\""
  testEvalExp []                       "(1 + 5)"
  testEvalExp []                       "((1 + 2) + 5)"




trustMe :: Either a b -> b
trustMe (Right x) = x
trustMe (Left  _) = error "YOU LIED TO ME"

testEvalExp env s = print $ evalExp (Env env) $ trustMe $ parse s



testParser = do
  parseTest "12"
  parseTest "(12 identifier 12)"
  parseTest "((12 ident 15) otherIdent 0)"
  parseTest "((\"Muhh string\" ident 15) otherIdent 0)"
  parseTest "[arg1 (arg1 + arg2) arg2]"
  parseTest "(addNums = [arg1 (arg1 plus arg2) arg2])"
  --parseTest "[12 (\"hoiiii\") foo]"
