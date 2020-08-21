
import           Parse
import           Eval



main :: IO ()
main = do
  testEval
  --testParser


testEval = do
  testEvalExp [("a", VPrimitive (VNumber 12))] (ExpIdent (Ident "a"))
  testEvalExp [] (ExpLit (StringLit "a"))



testEvalExp env exp = print $ evalExp (Env env) exp



testParser = do
  parseTest "12"
  parseTest "(12 identifier 12)"
  parseTest "((12 ident 15) otherIdent 0)"
  parseTest "((\"Muhh string\" ident 15) otherIdent 0)"
  parseTest "[arg1 (arg1 + arg2) arg2]"
  parseTest "(addNums = [arg1 (arg1 plus arg2) arg2])"
  --parseTest "[12 (\"hoiiii\") foo]"
