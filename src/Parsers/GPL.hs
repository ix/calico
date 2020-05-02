{-# LANGUAGE RecordWildCards #-}

module Parsers.GPL where

import Data.Color         (RGB (..))
import Text.Parsec
    (many, many1, manyTill, optional, sepEndBy, sepEndBy1, skipMany, try, (<|>))
import Text.Parsec.Char
    (alphaNum, anyChar, char, endOfLine, noneOf, string, tab)
import Text.Parsec.String (Parser)

import Parsers.Common (Entry (..), Palette (..), number)

-- | Parser for the .gpl header constant.
header :: Parser String
header = string "GIMP Palette"

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
