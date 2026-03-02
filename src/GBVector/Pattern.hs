-- | SVG pattern fills and tileable pattern generators.
--
-- Patterns are 'Element' groups sized for tiling. Use with
-- 'patternDef' or render directly as repeating backgrounds.
--
-- All functions are pure — they generate 'Element' groups at a given
-- tile size that tile seamlessly when repeated.
module GBVector.Pattern
  ( -- * Grid Patterns
    dotGrid,
    lineGrid,
    crosshatch,
    checker,

    -- * Pattern Element
    patternDef,
    PatternConfig (..),
    defaultPatternConfig,
  )
where

import Data.Text (Text)
import GBVector.Color (Color)
import GBVector.Element
  ( Element (..),
    Fill (..),
  )
import GBVector.Types (V2 (..))

-- ---------------------------------------------------------------------------
-- Pattern Config
-- ---------------------------------------------------------------------------

-- | Configuration for an SVG pattern definition.
data PatternConfig = PatternConfig
  { patternId :: !Text,
    patternWidth :: !Double,
    patternHeight :: !Double
  }
  deriving (Show, Eq)

-- | Default pattern config: 10x10 tile.
defaultPatternConfig :: Text -> PatternConfig
defaultPatternConfig pid = PatternConfig pid defaultTileSize defaultTileSize

-- ---------------------------------------------------------------------------
-- Pattern Element
-- ---------------------------------------------------------------------------

-- | Wrap an element as an SVG pattern definition with the given config.
-- The element should tile within @(0, 0)@ to @(width, height)@.
patternDef :: PatternConfig -> Element -> Element
patternDef config content =
  EGroup
    [ EWithId (patternId config) content
    ]

-- ---------------------------------------------------------------------------
-- Grid Patterns
-- ---------------------------------------------------------------------------

-- | A grid of dots. Each dot is a small circle.
--
-- @dotGrid spacing radius color@ produces dots spaced @spacing@ apart
-- with the given @radius@ and @color@.
dotGrid :: Double -> Double -> Color -> Element
dotGrid spacing radius color =
  let cols = ceiling (defaultTileSize / spacing) :: Int
      rows = ceiling (defaultTileSize / spacing) :: Int
      dots =
        [ ETranslate
            (fromIntegral c * spacing)
            (fromIntegral r * spacing)
            (EFill (SolidFill color) (ECircle radius))
        | r <- [0 .. rows],
          c <- [0 .. cols]
        ]
   in EGroup dots

-- | Horizontal and vertical lines forming a grid.
--
-- @lineGrid spacing strokeWidth color@ produces grid lines @spacing@
-- apart with the given @strokeWidth@ and @color@.
lineGrid :: Double -> Double -> Color -> Element
lineGrid spacing strokeWidth color =
  let size = defaultTileSize
      cols = ceiling (size / spacing) :: Int
      rows = ceiling (size / spacing) :: Int
      verticals =
        [ EStroke color strokeWidth $
            ELine (V2 (fromIntegral c * spacing) 0) (V2 (fromIntegral c * spacing) size)
        | c <- [0 .. cols]
        ]
      horizontals =
        [ EStroke color strokeWidth $
            ELine (V2 0 (fromIntegral r * spacing)) (V2 size (fromIntegral r * spacing))
        | r <- [0 .. rows]
        ]
   in EGroup (verticals ++ horizontals)

-- | Diagonal crosshatch pattern.
--
-- @crosshatch spacing strokeWidth color@ produces crossing diagonal lines
-- at 45 degrees, @spacing@ apart.
crosshatch :: Double -> Double -> Color -> Element
crosshatch spacing strokeWidth color =
  let size = defaultTileSize
      count = ceiling (size / spacing) :: Int
      forwardSlash =
        [ EStroke color strokeWidth $
            ELine
              (V2 (fromIntegral i * spacing - size) 0)
              (V2 (fromIntegral i * spacing) size)
        | i <- [0 .. count * 2]
        ]
      backSlash =
        [ EStroke color strokeWidth $
            ELine
              (V2 (fromIntegral i * spacing - size) size)
              (V2 (fromIntegral i * spacing) 0)
        | i <- [0 .. count * 2]
        ]
   in EGroup (forwardSlash ++ backSlash)

-- | Checkerboard pattern.
--
-- @checker cellSize color1 color2@ produces alternating squares of
-- @cellSize@ in the two colors.
checker :: Double -> Color -> Color -> Element
checker cellSize color1 color2 =
  let cols = ceiling (defaultTileSize / cellSize) :: Int
      rows = ceiling (defaultTileSize / cellSize) :: Int
      cells =
        [ ETranslate (fromIntegral c * cellSize) (fromIntegral r * cellSize) $
            EFill (SolidFill (if even (c + r) then color1 else color2)) $
              ERect cellSize cellSize
        | r <- [0 .. rows - 1],
          c <- [0 .. cols - 1]
        ]
   in EGroup cells

-- ---------------------------------------------------------------------------
-- Internal
-- ---------------------------------------------------------------------------

-- | Default tile size for pattern generators.
defaultTileSize :: Double
defaultTileSize = 100.0
