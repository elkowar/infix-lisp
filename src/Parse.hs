module Parse
 ( NExp(..)
 , NLiteral(..)
 , NIdent(..)
 , NLambda(..)
 , parseTest
 , parse
 )
where

import qualified Text.Parsec                   as P
import           Text.Parsec                    ( (<?>) )
import           Data.Functor                   ( ($>) )



data NExp = ExpInvocation NExp NIdent NExp | ExpLambda NLambda | ExpLit NLiteral | ExpIdent NIdent deriving (Eq)
data NLiteral = IntLit Int | StringLit String deriving (Eq)
newtype NIdent = Ident String deriving (Eq)
data NLambda = Lambda NIdent NExp NIdent deriving (Eq)

instance Show NIdent where
 show (Ident s) = s

instance Show NLiteral where
 show (IntLit    num) = show num
 show (StringLit str) = show str

instance Show NLambda where
 show (Lambda arg1 body arg2) =
  unwords ["(", show arg1, show body, show arg2, ")"]

instance Show NExp where
 show (ExpLambda lambda) = show lambda
 show (ExpInvocation exp1 ident exp2) =
  unwords ["(", show exp1, show ident, show exp2, ")"]
 show (ExpLit   lit  ) = show lit
 show (ExpIdent ident) = show ident


type Parser a = P.Parsec String () a

parseTest :: String -> IO ()
parseTest = P.parseTest parseExp

parse = P.runParser parseExp () ""

parseExp :: Parser NExp
parseExp = P.choice
 [ P.try (ExpLambda <$> parseLambda)
 , P.try (ExpLit <$> parseLit)
 , P.try parseExpInvocation
 , P.try (ExpIdent <$> parseIdent)
 ]
 where
  parseExpInvocation :: Parser NExp
  parseExpInvocation = named "invocation block" $ do
   P.char '('
   P.spaces
   invo <-
    ExpInvocation
    <$> parseExp
    <*> surroundedBy (P.skipMany1 P.space) parseIdent
    <*> parseExp
   P.spaces
   P.char ')'
   pure invo

parseLambda :: Parser NLambda
parseLambda = named "lambda" $ do
 P.char '['
 P.spaces
 lambda <-
  Lambda
  <$> (parseIdent <?> "identifier")
  <*> surroundedBy (P.skipMany1 P.space) parseExp
  <*> (parseIdent <?> "identifier")
 P.spaces
 P.char ']'
 pure lambda


parseIdent :: Parser NIdent
parseIdent = named "identifier" $ P.choice
 [ Ident <$> ((:) <$> P.letter <*> P.many P.alphaNum)
 , Ident <$> P.many1 (P.oneOf "/+.-*=$!%&,")
 ]


parseLit :: Parser NLiteral
parseLit = named "literal" $ P.choice [IntLit <$> parseInt, parseStringLit]
 where
  parseStringLit :: Parser NLiteral
  parseStringLit =
   StringLit <$> (symbol '"' *> P.manyTill P.anyChar (P.try $ symbol '"'))

  parseInt :: Parser Int
  parseInt = read <$> P.many1 P.digit


symbol :: Char -> Parser ()
symbol c = P.char c $> ()

surroundedBy :: Parser () -> Parser a -> Parser a
surroundedBy surround p = surround *> p <* surround


named = flip P.label
