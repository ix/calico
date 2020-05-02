module Parsers.Ase (ase) where

import Data.Attoparsec.ByteString.Char8 (Parser)
import Parsers.Common        (Entry (..), Palette (..))

ase :: Parser Palette
ase = do

  pure Palette { colors = [], metadata = [] }
