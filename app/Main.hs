module Main where

import           Control.Monad
import           Data.Color
import           Data.Color.GPL
import           Data.List
import           System.Environment (getArgs)
import           Text.Parsec
import           Text.Parsec.Char
import           Text.Parsec.String
import           Text.Printf

-- A datatype representing commands in the Calico command language.
data Command = Hue Integer | Saturation Integer | Luminosity Integer
  deriving (Read, Show, Eq)

sign :: Parser Char
sign = char '-' <|> char '+'

signedNumber :: Parser Integer
signedNumber = do
  s <- sign
  num <- read <$> many1 digit
  return $ case s of
    '+' -> num
    '-' -> -num

seperator :: Parser Char
seperator = char ' ' <|> char ';' <|> char ',' <|> endOfLine

variable :: Parser String
variable = try (string "hue") <|> try (string "sat") <|> try (string "lum")

command :: Parser Command
command = do
  num <- signedNumber
  var <- variable
  return $ case var of
    "hue" -> Hue num
    "sat" -> Saturation num
    "lum" -> Luminosity num

main :: IO ()
main = getArgs >>= parseOpts

parseOpts :: [String] -> IO ()
parseOpts (filename:"--inline":_)  = printInline filename
parseOpts (filename:"--list":_)    = printNamedList filename
parseOpts (filename:"--grid":_)    = printGrid 5 3 filename
parseOpts (filename:"--hex":_)     = printHex filename
parseOpts (filename:"--verbose":_) = printVerbose filename
parseOpts ("--help":_)             = usage
parseOpts _                        = usage

usage :: IO ()
usage = do
  mapM_ printf [ "usage: palm [OPTIONS]\n"
               , "--inline display the colors in a single line\n"
               , "--list display the colors in a named list\n"
               , "--grid display the colors in a large grid\n"
               , "--hex display a hex list of the colors only\n"
               , "--verbose display hsl and hex conversions alongside the list view\n" ]

displayX :: Color a => Int -> a -> String
displayX n clr = concat $ take n $ repeat $ prints clr "██"

display :: Color a => a -> String
display clr = displayX 1 clr

printInline :: FilePath -> IO ()
printInline filename = parseAnd filename $ \palette -> do
      forM_ (colors palette) $ \(GPLColor c _) -> printf "%s" $ display c
      printf "\n"

printNamedList :: FilePath -> IO ()
printNamedList filename = parseAnd filename $ \palette -> do
      forM_ (colors palette) $ \(GPLColor c n) -> printf "%s %s\n" (display c) n

printGrid :: Int -> Int -> FilePath -> IO ()
printGrid count size filename = parseAnd filename $ \palette -> do
      forM_ (chunksOf count $ colors palette) $ \row -> do
        putStrLn $ unlines $ take size $ repeat $ intercalate " " $ map (displayX size . color) row

printHex :: FilePath -> IO ()
printHex filename = parseAnd filename $ \palette -> do
  forM_ (colors palette) $ \(GPLColor c _) -> do
    putStrLn $ toHex c

printVerbose :: FilePath -> IO ()
printVerbose filename = parseAnd filename $ \palette -> do
  forM_ (colors palette) $ \(GPLColor c n) -> do
    printf "%s %s %s %s %s\n" (display c) n (show c) (show $ rgb2hsl c) (toHex c)

parseAnd :: FilePath -> (Palette -> IO ()) -> IO ()
parseAnd filename action = do
  result <- parseGPLFile filename
  case result of
    Right palette -> action palette
    Left err -> do
      putStrLn "Couldn't parse palette."
      print err

chunksOf :: Int -> [a] -> [[a]]
chunksOf n xs = go [] n xs
  where go accum n [] = reverse accum
        go accum n xs = go (take n xs : accum) n (drop n xs)
