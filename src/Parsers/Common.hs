module Parsers.Common where

import Control.Applicative              ((<|>))
import Data.Attoparsec.ByteString.Char8
  (Parser, char, digit, isAlpha_ascii, many', many1, satisfy, sepBy, sepBy1)
import Data.Color                       (RGB)

-- | A generic color type with an optional name.
data Entry = Entry
  { color :: RGB
  , name  :: Maybe String
  }
  deriving (Read, Show, Eq)

-- | Representation a collection of colors and metadata.
data Palette = Palette
  { colors   :: [Entry]
  , metadata :: [(String, String)]
  }
  deriving (Read, Show, Eq)

-- | A sign for use in a signed integer.
sign :: Parser Char
sign = char '-' <|> char '+'

-- | A number, or >= 1 digit sequentially.
number :: Parser Integer
number = read <$> many1 digit

-- | A signed integer, with a mandatory +.
signedNumber :: Parser Integer
signedNumber = do
  s <- sign
  num <- number
  case s of
    '+' -> pure num
    '-' -> pure $ -num
    _   -> fail "lack of sign"

alphaNum :: Parser Char
alphaNum = satisfy isAlpha_ascii <|> digit

oneOf :: [Char] -> Parser Char
oneOf = satisfy . flip elem

noneOf :: [Char] -> Parser Char
noneOf xs = satisfy (not . (`elem` xs))

tab :: Parser Char
tab = char '\t'

sepEndBy :: Parser a -> Parser b -> Parser [a]
sepEndBy thing sep = do
  results <- thing `sepBy` sep
  many' sep
  pure results

sepEndBy1 :: Parser a -> Parser b -> Parser [a]
sepEndBy1 thing sep = do
  results <- thing `sepBy1` sep
  many' sep
  pure results
