{-# LANGUAGE NamedFieldPuns #-}

module Parsers.Hex (hexList) where

import Data.Color         (RGB (..))
import Data.Word          (Word8)
import Numeric            (readHex)
import Parsers.Common     (Entry (..), Palette (..))
import Text.Parsec
    (char, endOfLine, hexDigit, oneOf, optional, sepEndBy1, try, (<|>))
import Text.Parsec.String (Parser)

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

-- | A hex color list palette parser.
hexList :: Parser Palette
hexList = do
  colors <- try $ hexColor `sepEndBy1` (endOfLine <|> oneOf ", ;")
  pure Palette { colors, metadata = [] }
