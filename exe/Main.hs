{-# LANGUAGE ApplicativeDo     #-}
{-# LANGUAGE MultiWayIf        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Main where

import Control.Applicative              ((<|>))
import Control.Monad                    (forM_, void)
import Data.Attoparsec.ByteString.Char8
    (Parser, char, endOfLine, parseOnly, sepBy, string)
import Data.ByteString.Char8            (ByteString)
import Data.Maybe                       (fromMaybe)
import Text.Printf                      (printf)

import Data.Color
import Parsers        (parseFile, parseString)
import Parsers.Common (Entry (..), Palette (..), signedNumber)

import qualified Data.ByteString.Char8 as BS
import qualified Options.Applicative   as O

-- | The command line option state used by optparse-applicative.
data Options = Options
  { filename    :: String
  , inline      :: Bool
  , list        :: Bool
  , grid        :: Bool
  , hex         :: Bool
  , fmt         :: String
  , gridColumns :: Int
  , gridSize    :: Int
  , commands    :: ByteString
  }
  deriving (Read, Show)

-- | A datatype representing commands in the Calico command language.
data Command = Hue Integer
  | Saturation Integer
  | Luminosity Integer
  deriving (Read, Show, Eq)

-- | Any valid separator character of space, semicolon, EOL or comma.
seperator :: Parser ()
seperator = void (char ' ') <|> void (char ';') <|> void (char ',') <|> endOfLine

-- | The strings hue, sat or lum.
variable :: Parser ByteString
variable = string "hue" <|> string "sat" <|> string "lum"

-- | A full Command in the string representation.
command :: Parser Command
command = do
  num <- signedNumber
  var <- variable
  case var of
    "hue" -> pure $ Hue num
    "sat" -> pure $ Saturation num
    "lum" -> pure $ Luminosity num
    _     -> fail "couldn't parse command"

main :: IO ()
main = do
  options <- O.execParser (O.info (O.helper <*> opts) O.idm)
  parseResult <- if | filename options == "-" -> parseString <$> BS.getContents
                    | otherwise               -> parseFile (filename options)

  preparedPalette <- pure $
    if BS.null $ commands options then parseResult else do
      cmds <- parseOnly (command `sepBy` seperator) $ commands options
      transform cmds <$> parseResult

  case preparedPalette of
    Right palette ->
      if | inline options -> printInline palette
         | list options   -> printFormatted palette (fmt options)
         | grid options   -> printGrid (gridColumns options) (gridSize options) palette
         | hex options    -> printFormatted palette "x"
         | otherwise      -> printFormatted palette (fmt options)
    Left err -> putStrLn err


-- | optparse-applicative option parser.
opts :: O.Parser Options
opts = do
  filename <- O.strOption (O.short 'f' <> O.long "filename" <> O.value "-")
  inline <- O.switch (O.short 'i' <> O.long "inline" <> O.help "display the palette in a single line")
  list   <- O.switch (O.short 'l' <> O.long "list" <> O.help "display the palette as a list")
  grid   <- O.switch (O.short 'g' <> O.long "grid" <> O.help "display the palette as a grid")
  hex    <- O.switch (O.short 'x' <> O.long "hex" <> O.help "display a list of hex colors only")
  fmt    <- O.strOption (O.short 'f' <> O.long "format" <> O.value "cnrhx" <> O.help "the format string for --list")
  gridColumns <- O.option O.auto (O.short 'n' <> O.long "grid-columns" <> O.value 5 <> O.help "the number of color columns to use with --grid")
  gridSize <- O.option O.auto (O.short 's' <> O.long "grid-size" <> O.value 2 <> O.help "the size of individual colors to use with --grid")
  commands <- BS.pack <$> O.strOption (O.short 'm' <> O.long "modify" <> O.value "" <> O.help "a list of color modification commands")
  pure Options {..}

-- | It's just map flipped™️.
for :: [a] -> (a -> b) -> [b]
for = flip map

-- | Apply a series of commands to all colors in a palette.
transform :: [Command] -> Palette -> Palette
transform cmds palette = Palette { metadata = metadata palette, colors = newColors }
  where newColors = for (colors palette) $ \(Entry rgb n) -> Entry (foldr modify rgb cmds) n

-- | Apply a HSL command to an RGB color.
modify :: Command -> RGB -> RGB
modify cmd rgb =
  hsl2rgb $ case cmd of
    Hue n        -> HSL (deg $ d + n) s l
    Saturation n -> HSL h (s + fromIntegral n) l
    Luminosity n -> HSL h s (l + fromIntegral n)
  where (HSL h@(Degrees d) s l) = rgb2hsl rgb

-- | Get a colored square of arbitrary size.
displayX :: Color a => Int -> a -> String
displayX n clr = prints clr $ concat $ replicate n "██"

-- | Convenience function for displayX that pures a single square.
display :: Color a => a -> String
display = displayX 1

-- | Print all colors in a palette inline.
printInline :: Palette -> IO ()
printInline palette = do
      forM_ (colors palette) $ \(Entry c _) -> printf "%s" $ display c
      printf "\n"

-- | Print all colors in a palette in a grid, with `count` columns of squares `size` in diameter.
printGrid :: Int -> Int -> Palette -> IO ()
printGrid count size palette =
     forM_ (chunksOf count $ colors palette) $ \row ->
      putStrLn $ unlines $ replicate size $ unwords $ map (displayX size . color) row

-- | Print the colors in a palette in a formatted list.
printFormatted :: Palette -> String -> IO ()
printFormatted palette fmt =
  forM_ (colors palette) $ \col ->
    putStrLn $ unwords $ map (formatter col) fmt
  where
    formatter col 'c' = display (color col)
    formatter col 'n' = fromMaybe "Untitled" (name col)
    formatter col 'r' = show $ color col
    formatter col 'h' = show $ rgb2hsl $ color col
    formatter col 'x' = toHex $ color col
    formatter _ _     = ""

-- | Split a list into chunks.
chunksOf :: Int -> [a] -> [[a]]
chunksOf = go []
  where go accum _ [] = reverse accum
        go accum n xs = go (take n xs : accum) n (drop n xs)
