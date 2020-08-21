module Parse
  ( NExp(..)
  , NLiteral(..)
  , NIdent(..)
  , parseTest
  , parse
  )
where

import qualified Text.Megaparsec               as P
import qualified Text.Megaparsec.Char          as P
import qualified Text.Megaparsec.Char.Lexer    as L
import           Text.Megaparsec                ( (<?>) )
import           Text.Megaparsec.Debug          ( dbg )
import           Data.Functor                   ( ($>)
                                                , void
                                                )
import           Data.Void                      ( Void )
import           Control.Applicative            ( (<|>) )



data NExp = ExpInvocation NExp NExp NExp
          | ExpLambda NIdent NExp NIdent
          | ExpLit NLiteral
          | ExpIdent NIdent
          deriving (Eq)

data NLiteral = IntLit Int
              | StringLit String
              | NilLit deriving (Eq)
newtype NIdent = Ident String deriving (Eq)

instance Show NIdent where
  show (Ident s) = s

instance Show NLiteral where
  show (IntLit    num) = show num
  show (StringLit str) = show str
  show NilLit          = "nil"


instance Show NExp where
  show (ExpInvocation exp1 ident exp2) =
    unwords ["(", show exp1, show ident, show exp2, ")"]
  show (ExpLambda arg1 body arg2) =
    unwords ["[", show arg1, show body, show arg2, "]"]
  show (ExpLit   lit  ) = show lit
  show (ExpIdent ident) = show ident


type Parser = P.Parsec Void String

parseTest :: String -> IO ()
parseTest = P.parseTest parseExp

parse = P.runParser parseExp ""


parseExp :: Parser NExp
parseExp = P.choice
  [ P.try parseExpInvocation
  , P.try parseLambda
  , P.try (ExpLit <$> parseLit)
  , P.try (ExpIdent <$> parseIdent)
  ]
 where
  parseExpInvocation :: Parser NExp
  parseExpInvocation = P.label "invocation block" $ do
    P.char '('
    ignoreSpaces
    invo <-
      ExpInvocation
      <$> parseExp
      <*> surroundedBy (P.some P.spaceChar) (P.try parseExp)
      <*> parseExp
    ignoreSpaces
    P.char ')'
    pure invo

  parseLambda :: Parser NExp
  parseLambda = P.label "lambda" $ dbg "lambda" $ do
    P.char '['
    ignoreSpaces
    lambda <-
      ExpLambda
      <$> (parseIdent <?> "identifier")
      <*> surroundedBy (P.some P.spaceChar) parseExp
      <*> (parseIdent <?> "identifier")
    ignoreSpaces
    P.char ']'
    pure lambda


parseIdent :: Parser NIdent
parseIdent = P.label "identifier" $ P.choice
  [ Ident <$> ((:) <$> P.letterChar <*> P.many P.alphaNumChar)
  , Ident <$> P.some (P.oneOf "/+.-*=$!%&,_")
  ]


parseLit :: Parser NLiteral
parseLit = P.label "literal"
  $ P.choice [IntLit <$> parseInt, parseStringLit, NilLit <$ P.string "nil"]
 where
  parseStringLit :: Parser NLiteral
  parseStringLit =
    StringLit <$> (P.char '"' *> P.manyTill L.charLiteral (P.char '"'))

  parseInt :: Parser Int
  parseInt = L.signed sc L.decimal


ignoreSpaces :: Parser ()
ignoreSpaces = P.skipMany P.spaceChar

surroundedBy :: Parser s -> Parser a -> Parser a
surroundedBy surround p = surround *> p <* surround


sc = L.space (void P.spaceChar)
             (L.skipLineComment "//")
             (L.skipBlockComment "/*" "*/")

