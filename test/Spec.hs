{-# LANGUAGE OverloadedStrings #-}

import Data.Color
import Data.Either
import Data.Word
import Data.Attoparsec.ByteString.Char8 (parseOnly)
import Parsers
import Parsers.Common
import Parsers.GPL
import Test.Hspec
import Test.QuickCheck

import qualified Data.ByteString.Char8 as BS

instance Arbitrary RGB where
  arbitrary = do
    r <- arbitrary
    g <- arbitrary
    b <- arbitrary
    return $ RGB r g b

-- | The margin of variation when converting RGB<->HSL.
-- I've checked against other implemenations and found at most 2 values can be off.
errorMargin :: Integer
errorMargin = 3

-- | Tests if two RGB colors are within an acceptable margin of difference.
(=~) (RGB r g b) (RGB r' g' b') = constrain r r' && constrain g g' && constrain b b'
  where constrain a b = (distance (fromIntegral a) (fromIntegral b)) <= errorMargin

-- | Evaluate the distance between two numbers.
distance :: Num a => a -> a -> a
distance a b = abs $ a - b

-- | A property that tests if a color is roughly
-- equivalent after being converted to HSL and back.
prop_hsl_roughly_equivalent :: RGB -> Bool
prop_hsl_roughly_equivalent color = color =~ (hsl2rgb $ rgb2hsl color)

-- | A property that tests that numbers are parsed
-- correctly by the GPL format parser. We use Word64
-- here because we know negative integers fail and that's appropriate.
prop_parses_number :: Word64 -> Bool
prop_parses_number n =
  case parseOnly number (BS.pack $ show n) of
    Right n' -> fromIntegral n' == n
    Left  _  -> False

-- | A property that tests mandatorily signed numbers are parseable.
prop_parses_signed :: Integer -> Bool
prop_parses_signed n
  | n == 0 = doParse "+0" && doParse "-0"
  | n < 0  = doParse $ BS.pack $ show n
  | n > 0  = doParse $ mconcat ["+", BS.pack $ show n]
  where doParse m =
          case parseOnly signedNumber m of
            Right m' -> m' == n
            Left _   -> False

-- | A convenience function that truncates the floating point values in a HSL value.
-- We use this for comparison as float comparisons are unwieldy.
truncateHSL :: HSL -> HSL
truncateHSL (HSL d s l) = HSL d (fromIntegral $ truncate s) (fromIntegral $ truncate l)

main :: IO ()
main = do
  quickCheck (withMaxSuccess 10000 prop_hsl_roughly_equivalent)
  hspec $ do
    describe "Data.Color.rgb2hsl" $ do
      it "converts a known value correctly" $ do
        (truncateHSL $ rgb2hsl (RGB 250 19 8)) `shouldBe` HSL (deg 3) 96 50
    describe "Data.Color.GPL.number" $ do
      it "parses integers correctly" $ do
        quickCheck (withMaxSuccess 10000 prop_parses_number)
    describe "Data.Color.GPL.gpl" $ do
      it "parses a palette correctly" $ do
        let paldata = "GIMP Palette\n0 0 0 Untitled\n"
        isRight (parseOnly gpl paldata) `shouldBe` True
      it "ignores comments correctly" $ do
        let paldata = "GIMP Palette\n# Comment\n # Another comment\n0 0 0 Test\n"
        isRight (parseOnly gpl paldata) `shouldBe` True
      it "parses metadata correctly" $ do
        let paldata = "GIMP Palette\n# Comment\nColors: RGBA\n255 255 255 Untitled\n"
        isRight (parseOnly gpl paldata) `shouldBe` True
    describe "Data.Color.GPL.signedNumber" $ do
      it "parses mandatorily signed integers correctly" $ do
        quickCheck (withMaxSuccess 10000 prop_parses_signed)
