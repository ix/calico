module Data.Color.GPL where

import           Control.Monad.IO.Class
import           Data.Color
import           Text.Parsec
import           Text.Parsec.Char
import           Text.Parsec.String

data GPLColor = GPLColor { color :: RGB
                         , name  :: String }
                         deriving (Read, Show, Eq)

data Palette = Palette { colors   :: [GPLColor]
                       , metadata :: [(String, String)] }
                       deriving (Read, Show, Eq)

header :: Parser ()
header = do
  string "GIMP Palette"
  endOfLine
  return ()

number :: Parser String
number = many1 digit

whitespace :: Parser Char
whitespace = char ' ' <|> tab

commentLine :: Parser ()
commentLine = do
  skipMany whitespace
  char '#'
  manyTill anyChar endOfLine
  return ()

metadataLine :: Parser (String, String)
metadataLine = do
  key   <- manyTill anyChar (char ':')
  value <- manyTill anyChar endOfLine
  return (key, value)

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

palette :: Parser Palette
palette = do
  header
  md <- many $ try metadataLine
  skipMany $ try commentLine
  cols <- many $ try colorLine
  eof
  return $ Palette { colors = cols, metadata = md }

parseGPLFile :: FilePath -> IO (Either ParseError Palette)
parseGPLFile = parseFromFile palette
