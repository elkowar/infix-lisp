{-# OPTIONS_GHC -Wall -fno-warn-unused-imports -fno-warn-missing-signatures #-}
module Parse
  ( NExp(..)
  , NLiteral(..)
  , NIdent(..)
  , parseTest
  , parse
  )
where

import           Prelude                 hiding ( exp )
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

data NExp = ExpInBinding [(NIdent, NExp)] NExp
          | ExpCondition NExp NExp NExp
          | ExpInvocation NExp NExp NExp
          | ExpLambda NIdent NExp NIdent
          | ExpLit NLiteral
          | ExpIdent NIdent
          deriving (Eq)

data NLiteral = IntLit Int
              | StringLit String
              | BoolLit Bool
              | NilLit deriving (Eq)

data NIdent = Ident String | IdentIgnored deriving (Eq)

instance Show NIdent where
  show (Ident s)    = s
  show IdentIgnored = "_"

instance Show NLiteral where
  show (IntLit    num ) = show num
  show (StringLit str ) = show str
  show (BoolLit   bool) = if bool then "true" else "false"
  show NilLit           = "nil"


instance Show NExp where
  show (ExpCondition yesBody cond noBody) =
    unwords ["(", show yesBody, "<", show cond, ">", show noBody, ")"]
  show (ExpInBinding bindings exp) =
    unwords ["( {", show bindings, "} in", show exp, ")"] -- todo cleanup "showBindings"
  show (ExpInvocation exp1 ident exp2) =
    unwords ["(", show exp1, show ident, show exp2, ")"]
  show (ExpLambda arg1 body arg2) =
    unwords ["[", show arg1, show body, show arg2, "]"]
  show (ExpLit   lit  ) = show lit
  show (ExpIdent ident) = show ident


type Parser = P.Parsec Void String

parseTest :: String -> IO ()
parseTest = P.parseTest parseExp

parse :: String -> Either (P.ParseErrorBundle String Void) NExp
parse = P.runParser (surroundedByMany hiddenSpaceChar parseExp) ""



parseExp :: Parser NExp
parseExp = P.choice
  [ P.try parseInBinding
  , P.try parseConditional
  , P.try parseExpInvocation
  , parseList
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
      <$> (parseExp <|> (ExpLit NilLit <$ P.char '_'))
      <*> surroundedBy (P.some hiddenSpaceChar) invokableExpBlock
      <*> (parseExp <|> (ExpLit NilLit <$ P.char '_'))

  parseLambda :: Parser NExp
  parseLambda =
    P.label "lambda"
      $   brackets
      $   ExpLambda
      <$> (parseIdent <|> parseIgnoredIdent)
      <*> surroundedBy (P.some hiddenSpaceChar) parseExp
      <*> (parseIdent <|> parseIgnoredIdent)

  parseInBinding :: Parser NExp
  parseInBinding = P.label "in-binding" $ parens
    (   ExpInBinding
    <$> parseMapLiteral
    <*  surroundedBySome hiddenSpaceChar (P.string "in")
    <*> parseExp
    )

  parseMapLiteral :: Parser [(NIdent, NExp)]
  parseMapLiteral =
    P.label "map-literal"
      $ curlies
      $ surroundedByMany hiddenSpaceChar
      $ P.sepEndBy1 parseSingleBinding
                    (surroundedByMany hiddenSpaceChar (P.char ','))
   where
    parseSingleBinding =
      (,)
        <$> parseIdent
        <*  surroundedBySome hiddenSpaceChar (P.char '=')
        <*> parseExp

  parseList :: Parser NExp
  parseList = do
    results <- P.label "list-expression" $ curlies $ surroundedByMany
      hiddenSpaceChar
      (parseExp `P.sepBy` surroundedByMany hiddenSpaceChar (P.char ','))
    pure $ listToTupleList results
   where
    listToTupleList [] = ExpLit NilLit
    listToTupleList (x : xs) =
      ExpInvocation x (ExpIdent (Ident ",")) (listToTupleList xs)

  parseConditional :: Parser NExp
  parseConditional =
    P.label "condition"
      $   parens
      $   ExpCondition
      <$> parseExp
      <*> surroundedBySome hiddenSpaceChar (betweenChars '<' '>' parseExp)
      <*> parseExp


  invokableExpBlock :: Parser NExp
  invokableExpBlock =
    P.choice [parseExpInvocation, parseLambda, ExpIdent <$> parseIdent]


parseIdent :: Parser NIdent
parseIdent = P.label "identifier" $ P.choice
  [ Ident <$> ((:) <$> P.letterChar <*> P.many P.alphaNumChar)
  , Ident <$> P.some (P.oneOf "/+.-*=$!%&,<>:")
  ]

parseIgnoredIdent :: Parser NIdent
parseIgnoredIdent = IdentIgnored <$ P.hidden (P.char '_')

parseLit :: Parser NLiteral
parseLit = P.label "literal" $ P.choice
  [ parseStringLit
  , parseBoolLit
  , IntLit <$> P.try parseInt
  , NilLit <$ P.string "nil"
  ]
 where
  parseStringLit :: Parser NLiteral
  parseStringLit =
    StringLit
      <$> (  (P.char '"' <|> P.char '\'')
          *> P.manyTill L.charLiteral (P.char '"' <|> P.char '\'')
          )

  parseBoolLit :: Parser NLiteral
  parseBoolLit = P.choice
    [BoolLit True <$ P.string "true", BoolLit False <$ P.string "false"]

  parseInt :: Parser Int
  parseInt = L.signed (pure ()) L.decimal


hiddenSpaceChar :: Parser Char
hiddenSpaceChar = P.hidden (P.spaceChar <|> parseComment)
 where
  parseComment =
    ' ' <$ (P.string "/*" *> P.manyTill L.charLiteral (P.string "*/"))


parens = betweenChars '(' ')' . surroundedByMany hiddenSpaceChar
brackets = betweenChars '[' ']' . surroundedByMany hiddenSpaceChar
curlies = betweenChars '{' '}' . surroundedByMany hiddenSpaceChar

surroundedBy :: Parser s -> Parser a -> Parser a
surroundedBy surround p = surround *> p <* surround

surroundedByMany :: Parser s -> Parser a -> Parser a
surroundedByMany surround p = P.skipMany surround *> p <* P.skipMany surround

surroundedBySome :: Parser s -> Parser a -> Parser a
surroundedBySome surround p = P.skipSome surround *> p <* P.skipSome surround

betweenChars :: Char -> Char -> Parser a -> Parser a
betweenChars l r = P.between (P.char l) (P.char r)
