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

    -- * HSL
    hsl,
    hsla,

    -- * OKLAB
    toOklab,
    fromOklab,
    lerpOklab,

    -- * Color Adjustments
    lighten,
    darken,
    saturate,
    desaturate,
    invert,

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
data Color
  = Color
      -- | Red channel.
      !Double
      -- | Green channel.
      !Double
      -- | Blue channel.
      !Double
      -- | Alpha channel.
      !Double
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

-- | Fully transparent (alpha 0).
transparent :: Color
transparent = Color 0 0 0 0

-- | Black (@#000000@).
black :: Color
black = Color 0 0 0 1

-- | White (@#ffffff@).
white :: Color
white = Color 1 1 1 1

-- | Red (@#ff0000@).
red :: Color
red = Color 1 0 0 1

-- | Green (@#008000@).
green :: Color
green = rgb8 0 128 0

-- | Blue (@#0000ff@).
blue :: Color
blue = Color 0 0 1 1

-- | Yellow (@#ffff00@).
yellow :: Color
yellow = Color 1 1 0 1

-- | Cyan (@#00ffff@).
cyan :: Color
cyan = Color 0 1 1 1

-- | Magenta (@#ff00ff@).
magenta :: Color
magenta = Color 1 0 1 1

-- ---------------------------------------------------------------------------
-- Named Colors — Grays
-- ---------------------------------------------------------------------------

-- | Gray (@#808080@).
gray :: Color
gray = rgb8 128 128 128

-- | Dark gray (@#a9a9a9@).
darkGray :: Color
darkGray = rgb8 169 169 169

-- | Light gray (@#d3d3d3@).
lightGray :: Color
lightGray = rgb8 211 211 211

-- | Silver (@#c0c0c0@).
silver :: Color
silver = rgb8 192 192 192

-- | Dim gray (@#696969@).
dimGray :: Color
dimGray = rgb8 105 105 105

-- ---------------------------------------------------------------------------
-- Named Colors — Reds & Pinks
-- ---------------------------------------------------------------------------

-- | Crimson (@#dc143c@).
crimson :: Color
crimson = rgb8 220 20 60

-- | Dark red (@#8b0000@).
darkRed :: Color
darkRed = rgb8 139 0 0

-- | Coral (@#ff7f50@).
coral :: Color
coral = rgb8 255 127 80

-- | Tomato (@#ff6347@).
tomato :: Color
tomato = rgb8 255 99 71

-- | Salmon (@#fa8072@).
salmon :: Color
salmon = rgb8 250 128 114

-- | Hot pink (@#ff69b4@).
hotPink :: Color
hotPink = rgb8 255 105 180

-- | Deep pink (@#ff1493@).
deepPink :: Color
deepPink = rgb8 255 20 147

-- | Pink (@#ffc0cb@).
pink :: Color
pink = rgb8 255 192 203

-- ---------------------------------------------------------------------------
-- Named Colors — Oranges & Browns
-- ---------------------------------------------------------------------------

-- | Orange (@#ffa500@).
orange :: Color
orange = rgb8 255 165 0

-- | Dark orange (@#ff8c00@).
darkOrange :: Color
darkOrange = rgb8 255 140 0

-- | Gold (@#ffd700@).
gold :: Color
gold = rgb8 255 215 0

-- | Chocolate (@#d2691e@).
chocolate :: Color
chocolate = rgb8 210 105 30

-- | Saddle brown (@#8b4513@).
saddleBrown :: Color
saddleBrown = rgb8 139 69 19

-- | Sienna (@#a0522d@).
sienna :: Color
sienna = rgb8 160 82 45

-- ---------------------------------------------------------------------------
-- Named Colors — Greens
-- ---------------------------------------------------------------------------

-- | Lime (@#00ff00@).
lime :: Color
lime = Color 0 1 0 1

-- | Dark green (@#006400@).
darkGreen :: Color
darkGreen = rgb8 0 100 0

-- | Forest green (@#228b22@).
forestGreen :: Color
forestGreen = rgb8 34 139 34

-- | Sea green (@#2e8b57@).
seaGreen :: Color
seaGreen = rgb8 46 139 87

-- | Olive (@#808000@).
olive :: Color
olive = rgb8 128 128 0

-- | Teal (@#008080@).
teal :: Color
teal = rgb8 0 128 128

-- ---------------------------------------------------------------------------
-- Named Colors — Blues & Purples
-- ---------------------------------------------------------------------------

-- | Navy (@#000080@).
navy :: Color
navy = rgb8 0 0 128

-- | Dark blue (@#00008b@).
darkBlue :: Color
darkBlue = rgb8 0 0 139

-- | Royal blue (@#4169e1@).
royalBlue :: Color
royalBlue = rgb8 65 105 225

-- | Steel blue (@#4682b4@).
steelBlue :: Color
steelBlue = rgb8 70 130 180

-- | Sky blue (@#87ceeb@).
skyBlue :: Color
skyBlue = rgb8 135 206 235

-- | Indigo (@#4b0082@).
indigo :: Color
indigo = rgb8 75 0 130

-- | Purple (@#800080@).
purple :: Color
purple = rgb8 128 0 128

-- | Violet (@#ee82ee@).
violet :: Color
violet = rgb8 238 130 238

-- | Plum (@#dda0dd@).
plum :: Color
plum = rgb8 221 160 221

-- ---------------------------------------------------------------------------
-- HSL
-- ---------------------------------------------------------------------------

-- | Opaque color from hue (degrees), saturation, lightness in @[0, 1]@.
hsl :: Double -> Double -> Double -> Color
hsl h s l = hsla h s l 1.0

-- | Color from hue (degrees), saturation, lightness, alpha in @[0, 1]@.
hsla :: Double -> Double -> Double -> Double -> Color
hsla h s l a =
  let sc = clampUnit s
      lc = clampUnit l
      chroma = (1.0 - abs (2.0 * lc - 1.0)) * sc
      hNorm = wrapHue h / hueSegment
      x = chroma * (1.0 - abs (fmod hNorm 2.0 - 1.0))
      m = lc - chroma / 2.0
      (r1, g1, b1) = hueToRgb hNorm chroma x
   in Color (clampUnit (r1 + m)) (clampUnit (g1 + m)) (clampUnit (b1 + m)) (clampUnit a)

-- ---------------------------------------------------------------------------
-- OKLAB
-- ---------------------------------------------------------------------------

-- | Convert an RGBA color to OKLAB (L, a, b). Alpha is preserved separately.
-- Returns (L, a, b) where L is lightness in @[0, 1]@, a and b are
-- chromatic components roughly in @[-0.5, 0.5]@.
toOklab :: Color -> (Double, Double, Double)
toOklab (Color r g b _) =
  let rl = srgbToLinear r
      gl = srgbToLinear g
      bl = srgbToLinear b
      l_ = cbrt (oklabM00 * rl + oklabM01 * gl + oklabM02 * bl)
      m_ = cbrt (oklabM10 * rl + oklabM11 * gl + oklabM12 * bl)
      s_ = cbrt (oklabM20 * rl + oklabM21 * gl + oklabM22 * bl)
      okL = oklabN00 * l_ + oklabN01 * m_ + oklabN02 * s_
      okA = oklabN10 * l_ + oklabN11 * m_ + oklabN12 * s_
      okB = oklabN20 * l_ + oklabN21 * m_ + oklabN22 * s_
   in (okL, okA, okB)

-- | Convert OKLAB (L, a, b) back to an RGBA color with alpha 1.
fromOklab :: Double -> Double -> Double -> Color
fromOklab okL okA okB =
  let l_ = okL + oklabInvN10 * okA + oklabInvN20 * okB
      m_ = okL + oklabInvN11 * okA + oklabInvN21 * okB
      s_ = okL + oklabInvN12 * okA + oklabInvN22 * okB
      l3 = l_ * l_ * l_
      m3 = m_ * m_ * m_
      s3 = s_ * s_ * s_
      rl = oklabInvM00 * l3 + oklabInvM01 * m3 + oklabInvM02 * s3
      gl = oklabInvM10 * l3 + oklabInvM11 * m3 + oklabInvM12 * s3
      bl = oklabInvM20 * l3 + oklabInvM21 * m3 + oklabInvM22 * s3
   in Color (clampUnit (linearToSrgb rl)) (clampUnit (linearToSrgb gl)) (clampUnit (linearToSrgb bl)) 1.0

-- | Perceptually uniform interpolation between two colors via OKLAB.
-- @t@ is clamped to @[0, 1]@. Alpha is interpolated linearly.
lerpOklab :: Double -> Color -> Color -> Color
lerpOklab t c1@(Color _ _ _ a1) c2@(Color _ _ _ a2) =
  let tc = clampUnit t
      (l1, ca1, cb1) = toOklab c1
      (l2, ca2, cb2) = toOklab c2
      mixed = fromOklab (lerpChannel tc l1 l2) (lerpChannel tc ca1 ca2) (lerpChannel tc cb1 cb2)
   in withAlpha (lerpChannel tc a1 a2) mixed

-- ---------------------------------------------------------------------------
-- Color Adjustments
-- ---------------------------------------------------------------------------

-- | Make a color lighter by the given factor in @[0, 1]@.
-- @lighten 0.2 c@ increases lightness by 20%.
lighten :: Double -> Color -> Color
lighten amount c =
  let (okL, okA, okB) = toOklab c
      (Color _ _ _ a) = c
   in withAlpha a (fromOklab (clampUnit (okL + clampUnit amount)) okA okB)

-- | Make a color darker by the given factor in @[0, 1]@.
-- @darken 0.2 c@ decreases lightness by 20%.
darken :: Double -> Color -> Color
darken amount c =
  let (okL, okA, okB) = toOklab c
      (Color _ _ _ a) = c
   in withAlpha a (fromOklab (clampUnit (okL - clampUnit amount)) okA okB)

-- | Increase color saturation (chroma) by the given factor.
-- @saturate 0.5 c@ boosts chroma by 50%. Output channels are clamped
-- to @[0, 1]@ via 'fromOklab', so the result always stays in gamut.
saturate :: Double -> Color -> Color
saturate amount c =
  let (okL, okA, okB) = toOklab c
      (Color _ _ _ a) = c
      chroma = sqrt (okA * okA + okB * okB)
      boosted = chroma * (1.0 + clampUnit amount)
      scaleFactor = if chroma > chromaEpsilon then boosted / chroma else 1.0
   in withAlpha a (fromOklab okL (okA * scaleFactor) (okB * scaleFactor))

-- | Decrease color saturation (chroma) by the given factor.
-- @desaturate 0.5 c@ reduces chroma by 50%.
desaturate :: Double -> Color -> Color
desaturate amount c =
  let (okL, okA, okB) = toOklab c
      (Color _ _ _ a) = c
      chroma = sqrt (okA * okA + okB * okB)
      reduced = chroma * (1.0 - clampUnit amount)
      scale = if chroma > chromaEpsilon then reduced / chroma else 1.0
   in withAlpha a (fromOklab okL (okA * scale) (okB * scale))

-- | Invert a color (complement). Alpha is preserved.
invert :: Color -> Color
invert (Color r g b a) = Color (1.0 - r) (1.0 - g) (1.0 - b) a

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

-- ---------------------------------------------------------------------------
-- HSL Internals
-- ---------------------------------------------------------------------------

-- | Degrees per HSL hue segment.
hueSegment :: Double
hueSegment = 60.0

-- | Wrap hue to @[0, 360)@.
wrapHue :: Double -> Double
wrapHue h
  | wrapped < 0 = wrapped + fullCircleDeg
  | otherwise = wrapped
  where
    wrapped = fmod h fullCircleDeg

-- | Full circle in degrees.
fullCircleDeg :: Double
fullCircleDeg = 360.0

-- | Floating-point modulus.
fmod :: Double -> Double -> Double
fmod a b = a - fromIntegral (floor (a / b) :: Int) * b

-- | Map a hue segment index to an (R, G, B) triple.
hueToRgb :: Double -> Double -> Double -> (Double, Double, Double)
hueToRgb hNorm c x
  | hNorm < 1 = (c, x, 0)
  | hNorm < 2 = (x, c, 0)
  | hNorm < 3 = (0, c, x)
  | hNorm < 4 = (0, x, c)
  | hNorm < 5 = (x, 0, c)
  | otherwise = (c, 0, x)

-- ---------------------------------------------------------------------------
-- OKLAB Internals
-- ---------------------------------------------------------------------------

-- | Epsilon for near-zero chroma checks.
chromaEpsilon :: Double
chromaEpsilon = 1.0e-10

-- | sRGB to linear conversion (gamma decode).
srgbToLinear :: Double -> Double
srgbToLinear c
  | c <= srgbThreshold = c / srgbLinearScale
  | otherwise = ((c + srgbOffset) / (1.0 + srgbOffset)) ** srgbGamma

-- | Linear to sRGB conversion (gamma encode).
linearToSrgb :: Double -> Double
linearToSrgb c
  | c <= srgbLinearThreshold = c * srgbLinearScale
  | otherwise = (1.0 + srgbOffset) * (c ** (1.0 / srgbGamma)) - srgbOffset

-- sRGB gamma constants
srgbThreshold :: Double
srgbThreshold = 0.04045

srgbLinearThreshold :: Double
srgbLinearThreshold = 0.0031308

srgbLinearScale :: Double
srgbLinearScale = 12.92

srgbOffset :: Double
srgbOffset = 0.055

srgbGamma :: Double
srgbGamma = 2.4

-- | Cube root that handles negative values.
cbrt :: Double -> Double
cbrt x
  | x >= 0 = x ** (1.0 / 3.0)
  | otherwise = -((-x) ** (1.0 / 3.0))

-- OKLAB forward matrix: linear RGB -> LMS (first stage)
oklabM00, oklabM01, oklabM02 :: Double
oklabM00 = 0.4122214708
oklabM01 = 0.5363325363
oklabM02 = 0.0514459929

oklabM10, oklabM11, oklabM12 :: Double
oklabM10 = 0.2119034982
oklabM11 = 0.6806995451
oklabM12 = 0.1073969566

oklabM20, oklabM21, oklabM22 :: Double
oklabM20 = 0.0883024619
oklabM21 = 0.2817188376
oklabM22 = 0.6299787005

-- OKLAB forward matrix: LMS^(1/3) -> Lab (second stage)
oklabN00, oklabN01, oklabN02 :: Double
oklabN00 = 0.2104542553
oklabN01 = 0.7936177850
oklabN02 = -0.0040720468

oklabN10, oklabN11, oklabN12 :: Double
oklabN10 = 1.9779984951
oklabN11 = -2.4285922050
oklabN12 = 0.4505937099

oklabN20, oklabN21, oklabN22 :: Double
oklabN20 = 0.0259040371
oklabN21 = 0.7827717662
oklabN22 = -0.8086757660

-- OKLAB inverse matrix: Lab -> LMS^(1/3) (second stage inverse)
oklabInvN10, oklabInvN11, oklabInvN12 :: Double
oklabInvN10 = 0.3963377774
oklabInvN11 = -0.1055613458
oklabInvN12 = -0.0894841775

oklabInvN20, oklabInvN21, oklabInvN22 :: Double
oklabInvN20 = 0.2158037573
oklabInvN21 = -0.0638541728
oklabInvN22 = -1.2914855480

-- OKLAB inverse matrix: LMS -> linear RGB (first stage inverse)
oklabInvM00, oklabInvM01, oklabInvM02 :: Double
oklabInvM00 = 4.0767416621
oklabInvM01 = -3.3077115913
oklabInvM02 = 0.2309699292

oklabInvM10, oklabInvM11, oklabInvM12 :: Double
oklabInvM10 = -1.2684380046
oklabInvM11 = 2.6097574011
oklabInvM12 = -0.3413193965

oklabInvM20, oklabInvM21, oklabInvM22 :: Double
oklabInvM20 = -0.0041960863
oklabInvM21 = -0.7034186147
oklabInvM22 = 1.7076147010
