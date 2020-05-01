{-# LANGUAGE RecordWildCards #-}
module Data.Color.Parsers where

import Data.Color         (RGB (..))
import Data.Word          (Word8)
import Numeric            (readHex)
import Text.Parsec
    (ParseError, many, many1, manyTill, optional, sepEndBy, sepEndBy1, skipMany, try, (<|>))
import Text.Parsec.Char
    (alphaNum, anyChar, char, digit, endOfLine, hexDigit, noneOf, oneOf, string, tab)
import Text.Parsec.String (Parser, parseFromFile)

-- A generic color type with an optional name.
data Entry = Entry
  { color :: RGB
  , name  :: Maybe String
  }
  deriving (Read, Show, Eq)

-- | Representation of a .gpl file - a collection of colors and metadata.
data Palette = Palette
  { colors   :: [Entry]
  , metadata :: [(String, String)]
  }
  deriving (Read, Show, Eq)

-- | Parser for the .gpl header constant.
header :: Parser String
header = string "GIMP Palette"

-- | A pair of hex digits (e.g. FF).
hexWord8 :: Parser Word8
hexWord8 = do
  a <- hexDigit
  b <- hexDigit
  pure $ fst . head $ readHex [a, b]

-- | A hex color. Doesn't support single character color components.
hexColor :: Parser Entry
hexColor = do
  optional $ char '#'
  r <- hexWord8
  g <- hexWord8
  b <- hexWord8
  pure Entry { color = RGB r g b, name = Nothing }

-- | A number, or >= 1 digit sequentially.
number :: Parser Integer
number = read <$> many1 digit

-- | A sign for use in a signed integer.
sign :: Parser Char
sign = char '-' <|> char '+'

-- | A signed integer, with a mandatory +.
signedNumber :: Parser Integer
signedNumber = do
  s <- sign
  num <- number
  pure $ case s of
    '+' -> num
    '-' -> -num

-- | A space or tab character.
whitespace :: Parser Char
whitespace = char ' ' <|> tab

-- | A comment line.
commentLine :: Parser ()
commentLine = do
  skipMany whitespace
  char '#'
  many $ noneOf "\n\r"
  pure ()

-- | A metadata key/value line.
metadataLine :: Parser (String, String)
metadataLine = do
  key   <- manyTill anyChar (char ':')
  value <- manyTill anyChar endOfLine
  pure (key, value)

-- | A color entry.
colorLine :: Parser Entry
colorLine = do
  skipMany whitespace
  r <- fromIntegral <$> number
  skipMany whitespace
  g <- fromIntegral <$> number
  skipMany whitespace
  b <- fromIntegral <$> number
  skipMany whitespace
  -- in the case of RGBA
  optional $ do
    number
    skipMany whitespace
  name <- many1 (alphaNum <|> whitespace)
  pure $ Entry { color = RGB r g b, name = Just name }

-- | A .gpl palette parser.
gpl :: Parser Palette
gpl = do
  header
  endOfLine
  metadata <- (try metadataLine) `sepEndBy` endOfLine
  optional $ (try commentLine) `sepEndBy` endOfLine
  colors <- try $ colorLine `sepEndBy1` endOfLine
  pure Palette {..}

-- | A hex color list palette parser.
hexList :: Parser Palette
hexList = do
  colors <- try $ hexColor `sepEndBy1` (endOfLine <|> oneOf ", ;")
  pure Palette { colors = colors, metadata = [] }

-- | A convenience function to parse a .gpl palette from a file.
parseFile :: FilePath -> IO (Either ParseError Palette)
parseFile = parseFromFile (gpl <|> hexList)
