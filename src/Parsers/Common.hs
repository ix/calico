{-# LANGUAGE NumericUnderscores #-}

module Parsers.Common where

import Control.Applicative              ((<|>))
import Data.Attoparsec.ByteString.Char8
    (Parser, char, digit, isAlpha_ascii, many', many1, satisfy, sepBy, sepBy1,
    signed)
import Data.Bits                        (shiftL, (.|.))
import Data.ByteString                  (ByteString, unpack)
import Data.Color                       (RGB)
import Data.Function                    ((&))
import Data.Word                        (Word16, Word32)

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

-- | A number, or >= 1 digit sequentially.
number :: Parser Integer
number = read <$> many1 digit

-- | A signed number.
signedNumber :: Parser Integer
signedNumber = signed number

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

-- | A class for attempted interpretation of ByteStrings.
class Transmute a where
  transmute :: ByteString -> (Maybe a)

instance Transmute Word16 where
  transmute ws = case unpack ws of
    [x, y] -> 0x0000 .|. (fromIntegral x `shiftL` 8) .|. fromIntegral y & Just
    _      -> Nothing

instance Transmute Word32 where
  transmute ws = case unpack ws of
    [v, w, x, y] -> 0x0000_0000
      .|. (fromIntegral v `shiftL` 24)
      .|. (fromIntegral w `shiftL` 16)
      .|. (fromIntegral x `shiftL`  8)
      .|. fromIntegral y & Just
    _ -> Nothing


