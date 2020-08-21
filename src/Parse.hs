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



data NExp = ExpInBinding NIdent NExp NExp
          | ExpInvocation NExp NExp NExp
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
  show (ExpInBinding name value exp) =
    unwords ["( {", show name, "=", show value, "} in", show exp, ")"]
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
  [ P.try parseInBinding
  , P.try parseExpInvocation
  , parseLambda
  , ExpLit <$> parseLit
  , ExpIdent <$> parseIdent
  ]
 where
  parseExpInvocation :: Parser NExp
  parseExpInvocation =
    P.label "invocation block"
      $   parens
      $   ExpInvocation
      <$> parseExp
      <*> surroundedBy (P.some hiddenSpaceChar) invokableExpBlock
      <*> parseExp

  parseLambda :: Parser NExp
  parseLambda =
    P.label "lambda"
      $   brackets
      $   ExpLambda
      <$> (parseIdent <?> "identifier")
      <*> surroundedBy (P.some hiddenSpaceChar) parseExp
      <*> (parseIdent <?> "identifier")

  parseInBinding :: Parser NExp
  parseInBinding = P.label "in-binding" $ parens $ do
    (name, valueExp) <- parens $ do
      name <- parseIdent
      surroundedByMany hiddenSpaceChar (P.char '=')
      valueExp <- parseExp
      pure (name, valueExp)
    surroundedBy (P.some hiddenSpaceChar) $ P.string "in"
    body <- parseExp
    pure $ ExpInBinding name valueExp body


  invokableExpBlock :: Parser NExp
  invokableExpBlock =
    P.choice [parseExpInvocation, parseLambda, ExpIdent <$> parseIdent]


parseIdent :: Parser NIdent
parseIdent = P.label "identifier" $ P.choice
  [ Ident <$> ((:) <$> P.letterChar <*> P.many P.alphaNumChar)
  , Ident <$> P.some (P.oneOf "/+.-*=$!%&,_")
  ]


parseLit :: Parser NLiteral
parseLit = P.label "literal" $ P.choice
  [IntLit <$> P.try parseInt, parseStringLit, NilLit <$ P.string "nil"]
 where
  parseStringLit :: Parser NLiteral
  parseStringLit =
    StringLit <$> (P.char '"' *> P.manyTill L.charLiteral (P.char '"'))

  parseInt :: Parser Int
  parseInt = L.signed (pure ()) L.decimal


ignoreSpaces :: Parser ()
ignoreSpaces = P.skipMany hiddenSpaceChar

hiddenSpaceChar :: Parser Char
hiddenSpaceChar = P.hidden P.spaceChar

parens = P.between (P.char '(') (P.char ')') . surroundedByMany hiddenSpaceChar
brackets =
  P.between (P.char '[') (P.char ']') . surroundedByMany hiddenSpaceChar
curlies =
  P.between (P.char '{') (P.char '}') . surroundedByMany hiddenSpaceChar

surroundedBy :: Parser s -> Parser a -> Parser a
surroundedBy surround p = surround *> p <* surround

surroundedByMany :: Parser s -> Parser a -> Parser a
surroundedByMany surround p = P.skipMany surround *> p <* P.skipMany surround

sc :: Parser ()
sc = L.space (void hiddenSpaceChar)
             (L.skipLineComment "//")
             (L.skipBlockComment "/*" "*/")

