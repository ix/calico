module Parsers (parseFile, parseString) where

import Control.Applicative              ((<|>))
import Control.Monad                    ((>=>))
import Data.Attoparsec.ByteString.Char8 (parseOnly)
import Data.ByteString.Char8            (ByteString)
import Parsers.Ase                      (ase)
import Parsers.Common                   (Palette)
import Parsers.GPL                      (gpl)
import Parsers.Hex                      (hexList)

import qualified Data.ByteString.Char8 as BS

-- | A convenience function to parse a .gpl palette from a file.
parseFile :: FilePath -> IO (Either String Palette)
parseFile = BS.readFile >=> pure . parseString

parseString :: ByteString -> Either String Palette
parseString = parseOnly (gpl <|> ase <|> hexList)
