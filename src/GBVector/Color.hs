-- | RGBA color type and color math.
--
-- Colors use 'Double' channels in @[0, 1]@. Constructors clamp inputs.
-- Named colors follow the SVG/CSS specification.
module GBVector.Color
  ( -- * Types
    Color (..),

    -- * Construction
    rgb,
    rgba,
    rgb8,
    hex,

    -- * Manipulation
    withAlpha,
    lerp,
    toHex,

    -- * Named Colors — Basics
    transparent,
    black,
    white,
    red,
    green,
    blue,
    yellow,
    cyan,
    magenta,

    -- * Named Colors — Grays
    gray,
    darkGray,
    lightGray,
    silver,
    dimGray,

    -- * Named Colors — Reds & Pinks
    crimson,
    darkRed,
    coral,
    tomato,
    salmon,
    hotPink,
    deepPink,
    pink,

    -- * Named Colors — Oranges & Browns
    orange,
    darkOrange,
    gold,
    chocolate,
    saddleBrown,
    sienna,

    -- * Named Colors — Greens
    lime,
    darkGreen,
    forestGreen,
    seaGreen,
    olive,
    teal,

    -- * Named Colors — Blues & Purples
    navy,
    darkBlue,
    royalBlue,
    steelBlue,
    skyBlue,
    indigo,
    purple,
    violet,
    plum,
  )
where

import Data.Char (intToDigit, isDigit)

-- ---------------------------------------------------------------------------
-- Types
-- ---------------------------------------------------------------------------

-- | RGBA color with channels in @[0, 1]@.
data Color = Color !Double !Double !Double !Double
  deriving (Show, Eq, Ord)

-- ---------------------------------------------------------------------------
-- Construction
-- ---------------------------------------------------------------------------

-- | Opaque color from red, green, blue in @[0, 1]@.
rgb :: Double -> Double -> Double -> Color
rgb r g b = Color (clampUnit r) (clampUnit g) (clampUnit b) 1.0

-- | Color from red, green, blue, alpha in @[0, 1]@.
rgba :: Double -> Double -> Double -> Double -> Color
rgba r g b a = Color (clampUnit r) (clampUnit g) (clampUnit b) (clampUnit a)

-- | Opaque color from 8-bit channels in @[0, 255]@.
rgb8 :: Int -> Int -> Int -> Color
rgb8 r g b =
  Color
    (fromIntegral (clamp8 r) / channelMax)
    (fromIntegral (clamp8 g) / channelMax)
    (fromIntegral (clamp8 b) / channelMax)
    1.0

-- | Parse a hex color string. Supports @#RGB@, @#RGBA@, @#RRGGBB@, @#RRGGBBAA@
-- (with or without the leading @#@). Returns 'black' on invalid input.
hex :: String -> Color
hex ('#' : rest) = hex rest
hex [r, g, b] = hex [r, r, g, g, b, b]
hex [r, g, b, a] = hex [r, r, g, g, b, b, a, a]
hex [r1, r0, g1, g0, b1, b0] =
  Color
    (hexPair r1 r0 / channelMax)
    (hexPair g1 g0 / channelMax)
    (hexPair b1 b0 / channelMax)
    1.0
hex [r1, r0, g1, g0, b1, b0, a1, a0] =
  Color
    (hexPair r1 r0 / channelMax)
    (hexPair g1 g0 / channelMax)
    (hexPair b1 b0 / channelMax)
    (hexPair a1 a0 / channelMax)
hex _ = black

-- ---------------------------------------------------------------------------
-- Manipulation
-- ---------------------------------------------------------------------------

-- | Replace the alpha channel, leaving RGB unchanged.
withAlpha :: Double -> Color -> Color
withAlpha a (Color r g b _) = Color r g b (clampUnit a)

-- | Linear interpolation between two colors. @t@ is clamped to @[0, 1]@.
lerp :: Double -> Color -> Color -> Color
lerp t (Color r1 g1 b1 a1) (Color r2 g2 b2 a2) =
  let tc = clampUnit t
   in Color
        (lerpChannel tc r1 r2)
        (lerpChannel tc g1 g2)
        (lerpChannel tc b1 b2)
        (lerpChannel tc a1 a2)

-- | Convert a color to a hex string like @\"#rrggbb\"@ or @\"#rrggbbaa\"@.
toHex :: Color -> String
toHex (Color r g b a)
  | a >= 1.0 = '#' : hexByte ri ++ hexByte gi ++ hexByte bi
  | otherwise = '#' : hexByte ri ++ hexByte gi ++ hexByte bi ++ hexByte ai
  where
    ri = round (r * channelMax) :: Int
    gi = round (g * channelMax) :: Int
    bi = round (b * channelMax) :: Int
    ai = round (a * channelMax) :: Int

-- ---------------------------------------------------------------------------
-- Named Colors — Basics
-- ---------------------------------------------------------------------------

transparent :: Color
transparent = Color 0 0 0 0

black :: Color
black = Color 0 0 0 1

white :: Color
white = Color 1 1 1 1

red :: Color
red = Color 1 0 0 1

green :: Color
green = rgb8 0 128 0

blue :: Color
blue = Color 0 0 1 1

yellow :: Color
yellow = Color 1 1 0 1

cyan :: Color
cyan = Color 0 1 1 1

magenta :: Color
magenta = Color 1 0 1 1

-- ---------------------------------------------------------------------------
-- Named Colors — Grays
-- ---------------------------------------------------------------------------

gray :: Color
gray = rgb8 128 128 128

darkGray :: Color
darkGray = rgb8 169 169 169

lightGray :: Color
lightGray = rgb8 211 211 211

silver :: Color
silver = rgb8 192 192 192

dimGray :: Color
dimGray = rgb8 105 105 105

-- ---------------------------------------------------------------------------
-- Named Colors — Reds & Pinks
-- ---------------------------------------------------------------------------

crimson :: Color
crimson = rgb8 220 20 60

darkRed :: Color
darkRed = rgb8 139 0 0

coral :: Color
coral = rgb8 255 127 80

tomato :: Color
tomato = rgb8 255 99 71

salmon :: Color
salmon = rgb8 250 128 114

hotPink :: Color
hotPink = rgb8 255 105 180

deepPink :: Color
deepPink = rgb8 255 20 147

pink :: Color
pink = rgb8 255 192 203

-- ---------------------------------------------------------------------------
-- Named Colors — Oranges & Browns
-- ---------------------------------------------------------------------------

orange :: Color
orange = rgb8 255 165 0

darkOrange :: Color
darkOrange = rgb8 255 140 0

gold :: Color
gold = rgb8 255 215 0

chocolate :: Color
chocolate = rgb8 210 105 30

saddleBrown :: Color
saddleBrown = rgb8 139 69 19

sienna :: Color
sienna = rgb8 160 82 45

-- ---------------------------------------------------------------------------
-- Named Colors — Greens
-- ---------------------------------------------------------------------------

lime :: Color
lime = Color 0 1 0 1

darkGreen :: Color
darkGreen = rgb8 0 100 0

forestGreen :: Color
forestGreen = rgb8 34 139 34

seaGreen :: Color
seaGreen = rgb8 46 139 87

olive :: Color
olive = rgb8 128 128 0

teal :: Color
teal = rgb8 0 128 128

-- ---------------------------------------------------------------------------
-- Named Colors — Blues & Purples
-- ---------------------------------------------------------------------------

navy :: Color
navy = rgb8 0 0 128

darkBlue :: Color
darkBlue = rgb8 0 0 139

royalBlue :: Color
royalBlue = rgb8 65 105 225

steelBlue :: Color
steelBlue = rgb8 70 130 180

skyBlue :: Color
skyBlue = rgb8 135 206 235

indigo :: Color
indigo = rgb8 75 0 130

purple :: Color
purple = rgb8 128 0 128

violet :: Color
violet = rgb8 238 130 238

plum :: Color
plum = rgb8 221 160 221

-- ---------------------------------------------------------------------------
-- Internal
-- ---------------------------------------------------------------------------

channelMax :: Double
channelMax = 255.0

clampUnit :: Double -> Double
clampUnit x
  | x < 0 = 0
  | x > 1 = 1
  | otherwise = x

clamp8 :: Int -> Int
clamp8 x
  | x < 0 = 0
  | x > 255 = 255
  | otherwise = x

lerpChannel :: Double -> Double -> Double -> Double
lerpChannel t a b = a + t * (b - a)

hexDigit :: Char -> Int
hexDigit c
  | isDigit c = fromEnum c - fromEnum '0'
  | c >= 'a' && c <= 'f' = fromEnum c - fromEnum 'a' + 10
  | c >= 'A' && c <= 'F' = fromEnum c - fromEnum 'A' + 10
  | otherwise = 0

hexPair :: Char -> Char -> Double
hexPair hi lo = fromIntegral (hexDigit hi * 16 + hexDigit lo)

hexByte :: Int -> String
hexByte n =
  let clamped = max 0 (min 255 n)
      hi = clamped `div` 16
      lo = clamped `mod` 16
   in [intToDigit hi, intToDigit lo]
