module Sdam.Parser
  (
    -- Value
    pValue,
    ParsedValue(..),

    -- Path
    pPath,

    -- Running
    parse
  ) where

import Control.Monad
import Data.Void

import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

import Sdam.Core

type Parser e = Parsec e String

pWhitespace :: Ord e => Parser e ()
pWhitespace = L.space space1 pLineComment pBlockComment
  where
    pLineComment = L.skipLineComment "//"
    pBlockComment = L.skipBlockCommentNested "/*" "*/"

pLexeme :: Ord e => Parser e a -> Parser e a
pLexeme = L.lexeme pWhitespace

pSymbol :: Ord e => String -> Parser e ()
pSymbol s = void (L.symbol pWhitespace s)

pSynShape :: Ord e => Parser e SynShape
pSynShape = parseSynShape <$> some pSynShapeChar

pSynShapeChar :: Ord e => Parser e Char
pSynShapeChar = char '\\' *> pEscaped <|> satisfy (not . needsEscape)
  where
    pEscaped = anySingle >>= withEscaped
    withEscaped 'n' = return '\n'
    withEscaped c
      | needsEscape c = return c
      | otherwise = fail ("Bad escape: " ++ show c)
    needsEscape c =
      c `elem` ("\\\n " :: [Char])

--------------------------------------------------------------------------------
-- Value
--------------------------------------------------------------------------------

type ValueParseErr = Void

newtype ParsedValue = ParsedValue (Syn ParsedValue)
  deriving newtype Show

pValue :: Parser ValueParseErr ParsedValue
pValue = do
  shape <- pLexeme pSynShape
  fields <- count (length shape) pValue
  return (ParsedValue (synReconstruct shape fields))

--------------------------------------------------------------------------------
-- Value
--------------------------------------------------------------------------------

type PathParseErr = Void

pPath :: Parser PathParseErr Path
pPath = Path <$> sepBy1 pPathSegment (pSymbol "/")

pPathSegment :: Parser PathParseErr PathSegment
pPathSegment =
  pLexeme $ do
    shape <- pLexeme pSynShape
    i <- between (pSymbol "[") (pSymbol "]") L.decimal
    return (PathSegment shape (intToIndex i))
