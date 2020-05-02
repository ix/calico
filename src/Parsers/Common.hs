module Parsers.Common where

import Data.Color         (RGB)
import Text.Parsec        (char, digit, many1, (<|>), unexpected)
import Text.Parsec.String (Parser)

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
    _   -> unexpected "lack of sign"
