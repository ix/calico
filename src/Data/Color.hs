module Data.Color where

import           Data.Char   (chr, ord)
import           Data.Fixed
import           Data.Word
import           Text.Printf

-- | An RGB (Red, Green, Blue) representation of a color.
data RGB = RGB Word8 Word8 Word8
  deriving (Read, Eq, Ord)

data Degrees = Degrees Integer
  deriving (Read, Eq, Ord)

-- | A HSL (Hue, Saturation, Luminosity) representation of a color.
data HSL = HSL Degrees Percent Percent
  deriving (Read, Eq, Ord)

type Percent = Float

instance Show Degrees where
  show (Degrees n) = concat [show n, "°"]

instance Show HSL where
  show (HSL h s l) = concat ["HSL(", show h, ",", show s, "%,", show l, "%)"]

instance Show RGB where
  show (RGB r g b) = concat ["RGB(", show r, ",", show g, ",", show b, ")"]

-- | A general color typeclass.
class Color a where
  toHex   :: a -> String
  prints  :: a -> String -> String

instance Color RGB where
  toHex (RGB r g b) = mconcat ["#", printf "%02X" r, printf "%02X" g, printf "%02X" b]
  prints (RGB r g b) str = mconcat [esc, "[38;2;", r', ";", g', ";", b', "m", str, esc, "[0m"]
      where esc  = chr 0x1B : []
            (r', g', b') = (show r, show g, show b)

instance Color HSL where
  toHex hsl = toHex $ hsl2rgb hsl
  prints hsl str = prints (hsl2rgb hsl) str

-- | A smart constructor which cyclicly handles degrees.
deg :: Integer -> Degrees
deg n
  | n < 0     = Degrees $ (360 - abs n) `mod` 360
  | n > 359   = Degrees $ n `mod` 360
  | otherwise = Degrees n

-- | Converts an RGB value to a HSL one.
rgb2hsl :: RGB -> HSL
rgb2hsl (RGB red green blue) = HSL (deg $ round hue) (sat * 100) (lum * 100)
  where r'    = (fromIntegral $ red) / 255
        g'    = (fromIntegral $ green) / 255
        b'    = (fromIntegral $ blue) / 255
        cMax  = maximum [r', g', b']
        cMin  = minimum [r', g', b']
        delta = cMax - cMin
        lum   = (cMax + cMin) / 2
        hue
          | delta == 0  = 0
          | cMax  == r' = 60.0 * ((g' - b') / delta)
          | cMax  == g' = 60.0 * (2 + (b' - r') / delta)
          | cMax  == b' = 60.0 * (4 + (r' - g') / delta)
        sat
          | delta == 0  = 0
          | otherwise   = delta / (1 - (abs $ 2 * lum - 1))

-- | Converts a HSL value to an RGB one.
hsl2rgb :: HSL -> RGB
hsl2rgb (HSL (Degrees hue) sat lum) = RGB r g b
  where
        h = (fromIntegral hue) / 60
        s = sat / 100
        l = lum / 100
        c  = (1 - (abs $ 2 * l - 1)) * s
        x  = c * (1 - (abs $ h `mod'` 2 - 1))
        m = l - c / 2
        (r', g', b')
          | h >= 0 && h <= 1  = (c, x, 0)
          | h >= 1 && h <= 2 =  (x, c, 0)
          | h >= 2 && h <= 3  = (0, c, x)
          | h >= 3 && h <= 4  = (0, x, c)
          | h >= 4 && h <= 5  = (x, 0, c)
          | h >= 5 && h <= 6  = (c, 0, x)
          | otherwise = (0, 0, 0)
        (r, g, b) = (round $ (r' + m) * 255, round $ (g' + m) * 255, round $ (b' + m) * 255)

