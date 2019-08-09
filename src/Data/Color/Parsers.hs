{-# LANGUAGE RecordWildCards #-}
module Data.Color.Parsers where

import           Control.Monad.IO.Class
import           Data.Color
import           Text.Parsec
import           Text.Parsec.Char
import           Data.Word
import           Numeric                        ( readHex )
import           Text.Parsec.String

-- A generic color type with an optional name.
data Entry = Entry { color :: RGB
                   , name  :: Maybe String }
                   deriving (Read, Show, Eq)

-- | Representation of a .gpl file - a collection of colors and metadata.
data Palette = Palette { colors   :: [Entry]
                       , metadata :: [(String, String)] }
                       deriving (Read, Show, Eq)

-- | Parser for the .gpl header constant.
header :: Parser String
header = string "GIMP Palette"

-- | A pair of hex digits (e.g. FF).
hexWord8 :: Parser Word8
hexWord8 = do
  a <- hexDigit  
  b <- hexDigit
  return $ fst . head $ readHex [a, b]

-- | A hex color. Doesn't support single character color components.
hexColor :: Parser Entry
hexColor = do
  optional $ char '#'
  r <- hexWord8
  g <- hexWord8
  b <- hexWord8
  return Entry { color = RGB r g b, name = Nothing }

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
  return $ case s of
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
  many (alphaNum <|> whitespace)
  return ()

-- | A metadata key/value line.
metadataLine :: Parser (String, String)
metadataLine = do
  key   <- manyTill anyChar (char ':')
  value <- manyTill anyChar endOfLine
  return (key, value)

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
  return $ Entry { color = RGB r g b, name = Just name }

-- | A .gpl palette parser.
gpl :: Parser Palette
gpl = do
  header
  endOfLine
  metadata <- (try metadataLine) `sepEndBy` endOfLine 
  optional $ (try commentLine) `sepEndBy` endOfLine
  colors <- try $ colorLine `sepEndBy1` endOfLine
  return Palette {..}

-- | A hex color list palette parser.
hexList :: Parser Palette
hexList = do
  colors <- try $ hexColor `sepEndBy1` endOfLine 
  return Palette { colors = colors, metadata = [] }

-- | A convenience function to parse a .gpl palette from a file.
parseFile :: FilePath -> IO (Either ParseError Palette)
parseFile = parseFromFile (gpl <|> hexList) 