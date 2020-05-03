{-# LANGUAGE NamedFieldPuns #-}

module Parsers.Hex (hexList) where

import Control.Applicative              ((<|>))
import Control.Monad                    (void)
import Data.Attoparsec.ByteString.Char8
    (Parser, anyChar, char, char8, endOfLine, many', try)
import Data.Color                       (RGB (..))
import Data.Word                        (Word8)
import Numeric                          (readHex)
import Parsers.Common
    (Entry (..), Palette (..), oneOf, sepEndBy1)
import Safe

-- | A pair of hex digits (e.g. FF).
hexWord8 :: Parser Word8
hexWord8 = do
  a <- anyChar
  b <- anyChar
  maybe (fail "Couldn't read hex value.") (pure . fst) $ headMay $ readHex [a, b]

-- | A hex color. Doesn't support single character color components.
hexColor :: Parser Entry
hexColor = do
  many' $ char '#'
  r <- hexWord8
  g <- hexWord8
  b <- hexWord8
  pure Entry { color = RGB r g b, name = Nothing }

-- | A hex color list palette parser.
hexList :: Parser Palette
hexList = do
  colors <- try $ hexColor `sepEndBy1` (endOfLine <|> void (oneOf ", ;"))
  pure Palette { colors, metadata = [] }
