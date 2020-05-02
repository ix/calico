module Parsers (parseFile, parseString) where

import Parsers.Common     (Palette)
import Parsers.GPL        (gpl)
import Parsers.Hex        (hexList)
import Text.Parsec        (ParseError, parse, (<|>))
import Text.Parsec.String (parseFromFile)

-- | A convenience function to parse a .gpl palette from a file.
parseFile :: FilePath -> IO (Either ParseError Palette)
parseFile = parseFromFile (gpl <|> hexList)

parseString :: String -> String -> Either ParseError Palette
parseString = parse (gpl <|> hexList)
