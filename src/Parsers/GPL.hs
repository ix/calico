{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Parsers.GPL where

import Control.Applicative              ((<|>))
import Data.Attoparsec.ByteString.Char8
  (Parser, anyChar, char, endOfLine, many', many1, manyTill, sepBy, sepBy1, skipMany, string, try)
import Data.ByteString.Char8            (ByteString)
import Data.Color                       (RGB (..))

import Parsers.Common
  (Entry (..), Palette (..), alphaNum, noneOf, number, sepEndBy, sepEndBy1, tab)

-- | Parser for the .gpl header constant.
header :: Parser ByteString
header = string "GIMP Palette"

-- | A space or tab character.
whitespace :: Parser Char
whitespace = char ' ' <|> tab

-- | A comment line.
commentLine :: Parser ()
commentLine = do
  skipMany whitespace
  char '#'
  many' $ noneOf "\r\n"
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
  many' $ do
    number
    skipMany whitespace
  name <- many1 (alphaNum <|> whitespace)
  pure $ Entry { color = RGB r g b, name = Just name }

-- | A .gpl palette parser.
gpl :: Parser Palette
gpl = do
  header
  endOfLine
  metadata <- metadataLine `sepEndBy` endOfLine
  commentLine `sepEndBy` endOfLine
  colors <- colorLine `sepEndBy1` endOfLine
  pure Palette {..}
