module Data.Color.GPL where

import           Control.Monad.IO.Class
import           Data.Color
import           Text.Parsec
import           Text.Parsec.Char
import           Text.Parsec.String

-- | A named color as they'd appear in a .gpl file.
data GPLColor = GPLColor { color :: RGB
                         , name  :: String }
                         deriving (Read, Show, Eq)

-- | Representation of a .gpl file - a collection of colors and metadata.
data Palette = Palette { colors   :: [GPLColor]
                       , metadata :: [(String, String)] }
                       deriving (Read, Show, Eq)

-- | Parser for the header constant.
header :: Parser ()
header = do
  string "GIMP Palette"
  endOfLine
  return ()

-- | A number, or >= 1 digit sequentially.
number :: Parser String
number = many1 digit

-- | A space or tab character.
whitespace :: Parser Char
whitespace = char ' ' <|> tab

-- | A comment line.
commentLine :: Parser ()
commentLine = do
  skipMany whitespace
  char '#'
  manyTill anyChar endOfLine
  return ()

-- | A metadata key/value line.
metadataLine :: Parser (String, String)
metadataLine = do
  key   <- manyTill anyChar (char ':')
  value <- manyTill anyChar endOfLine
  return (key, value)

-- | A named (RGB) color entry.
colorLine :: Parser GPLColor
colorLine = do
  skipMany whitespace
  r <- read <$> number
  skipMany whitespace
  g <- read <$> number
  skipMany whitespace
  b <- read <$> number
  many whitespace
  -- in the case of RGBA
  optional $ do
    number
    many whitespace
  name <- manyTill anyChar endOfLine
  return $ GPLColor { color = RGB r g b, name = name }

-- | A .gpl palette parser.
palette :: Parser Palette
palette = do
  header
  md <- many $ try metadataLine
  skipMany $ try commentLine
  cols <- many $ try colorLine
  eof
  return $ Palette { colors = cols, metadata = md }

-- | A convenience function to parse a .gpl palette from a file.
parseGPLFile :: FilePath -> IO (Either ParseError Palette)
parseGPLFile = parseFromFile palette
