{-# LANGUAGE RecordWildCards #-}

module Parsers.Ase (ase) where

import Control.Monad              (void)
import Data.Attoparsec.ByteString (Parser, many', parseOnly, word8)
import Data.ByteString            (ByteString)
import Data.Color                 (RGB (..))
import Data.Function              ((&))
import Data.Maybe                 (fromMaybe)
import Data.Word                  (Word16, Word32)
import Parsers.Common             (Entry (..), Palette (..), transmute)
import Safe                       (headMay)

import qualified Data.Attoparsec.ByteString as P
import qualified Data.ByteString            as BS
import qualified Data.ByteString.Char8      as BSC

data Chunk = Chunk
  { cSize :: Word32
  , cType :: Word16
  , cData :: ByteString
  }
  deriving (Eq, Show)

magic :: Parser ()
magic = void (word8 0xe0 >> word8 0xa5)

frameMagic :: Parser ()
frameMagic = void (word8 0xfa >> word8 0xf1)

isPaletteType :: Chunk -> Bool
isPaletteType = (==) 0x2019 . cType

chunk :: Parser Chunk
chunk = do
  cSize <- fromMaybe 0 . transmute . BS.reverse <$> P.take 4
  cType <- fromMaybe 0 . transmute . BS.reverse <$> P.take 2
  cData <- P.take (fromIntegral cSize)
  pure Chunk {..}

entry :: Parser Entry
entry = do
  hasName <- (== Just 1) . (transmute :: ByteString -> Maybe Word16) <$> P.take 2
  r <- P.anyWord8
  g <- P.anyWord8
  b <- P.anyWord8
  P.anyWord8 -- alpha
  name <- if not hasName then pure Nothing else do
    len <- fromMaybe 0 . (transmute :: ByteString -> Maybe Word16) <$> P.take 2
    Just <$> (P.take $ fromIntegral len)
  pure Entry { color = RGB r g b, name = BSC.unpack <$> name }

palette :: Parser Palette
palette = do
  P.take 4 -- size
  P.take 4 -- first index
  P.take 4 -- last index
  P.take 8 -- reserved
  colors <- many' entry
  pure Palette { colors = colors, metadata = [] }

ase :: Parser Palette
ase = do
  P.take 4 -- filesize
  magic
  P.take 122 -- remainder of header
  P.take 4 -- frame size
  frameMagic
  P.take 10 -- remainder of frame header
  chunk' <- headMay <$> filter isPaletteType <$> many' chunk
  case chunk' of
    Nothing    -> fail "No palette chunk found, possible this is not an Aseprite *palette* file."
    Just chunk'' -> parseOnly palette (cData chunk'') &
      either (const $ fail "Failed to parse palette chunk!") pure
