<div align="center">
<h1>gb-vector</h1>
<p><strong>Pure Haskell SVG Generation</strong></p>
<p>Composable Element tree — no XML library, no IO, just pure functions building Text.</p>
<p><a href="#overview">Overview</a> · <a href="#architecture">Architecture</a> · <a href="#usage">Usage</a> · <a href="#api">API</a> · <a href="#example">Example</a></p>
<p>

[![CI](https://github.com/Gondola-Bros-Entertainment/gb-vector/actions/workflows/ci.yml/badge.svg)](https://github.com/Gondola-Bros-Entertainment/gb-vector/actions/workflows/ci.yml)
[![Hackage](https://img.shields.io/hackage/v/gb-vector.svg)](https://hackage.haskell.org/package/gb-vector)
![Haskell](https://img.shields.io/badge/haskell-GHC%209.6-purple)
[![License](https://img.shields.io/badge/license-BSD--3--Clause-blue)](LICENSE)

</p>
</div>

---

## Overview

gb-vector is a pure Haskell library for generating SVG. Define shapes, style them, compose them, and render — all with pure functions.

Companion to [gb-sprite](https://github.com/Gondola-Bros-Entertainment/gb-sprite) (procedural raster) and [gb-synth](https://github.com/Gondola-Bros-Entertainment/gb-synth) (procedural audio).

**Features:**
- `Element` recursive sum type — style and transforms are constructors wrapping children
- Compose via function application (`translate 50 50 $ fill gold $ circle 30`) or `(&)`
- Shapes: circle, ellipse, rect, rounded rect, polygon, star, arc, ring
- Path DSL: monadic builder with lineTo, cubicTo, quadTo, arcTo, closePath
- Transforms: translate, rotate, rotateAround, scale, skew
- Style: fill, stroke, opacity, clip, mask, blur, drop shadow
- Gradients: linear, radial, with stop helpers
- Bezier math: De Casteljau evaluation, subdivision, flattening, arc-to-cubic
- Text elements with font configuration
- Semigroup/Monoid composition on Element
- Pure SVG serialization to Text
- File output via `writeSvg`

**Dependencies:** `base` + `text` only. Both GHC boot libraries. Zero external deps.

---

## Architecture

```
src/GBVector/
├── Types.hs       V2, Segment, ArcParams, Path, enums (LineCap, LineJoin, FillRule)
├── Color.hs       RGBA (Double 0-1), rgb/rgb8/hex, 39 named colors, lerp, toHex
├── Element.hs     Element tree, Fill, Gradient, StrokeConfig, FilterKind, Document
├── Path.hs        PathBuilder DSL (startAt/lineTo/cubicTo/closePath/buildPath)
├── Bezier.hs      De Casteljau, subdivision, bbox, arc-to-cubic, flatten, length
├── Shape.hs       circle, rect, roundedRect, ellipse, polygon, star, arc, ring
├── Gradient.hs    linearGradient, radialGradient, stop, evenStops
├── Transform.hs   translate, rotate, rotateAround, scale, skewX, skewY
├── Style.hs       fill, stroke, opacity, clip, mask, blur, dropShadow, withId, use
├── Compose.hs     group, empty, document, documentWithViewBox, background
├── Text.hs        text, textAt, textWithConfig, font config builders
└── SVG.hs         render :: Document -> Text, writeSvg :: FilePath -> Document -> IO ()
```

### Pipeline

```
Shapes/Paths → Style/Transform → Compose → Document → render → Text/SVG file
```

---

## Usage

### As a dependency

Add to your `.cabal` file:

```cabal
build-depends: gb-vector >= 0.1
```

### Generating SVG

```haskell
import GBVector.Color (gold)
import GBVector.Compose (document)
import GBVector.SVG (writeSvg)
import GBVector.Shape (star)
import GBVector.Style (fill)
import GBVector.Transform (translate)

main :: IO ()
main = writeSvg "star.svg" $
  document 200 200 $
    translate 100 100 $
      fill gold $
        star 5 80 35
```

---

## API

### Color

```haskell
data Color = Color !Double !Double !Double !Double  -- RGBA [0,1]

rgb       :: Double -> Double -> Double -> Color
rgba      :: Double -> Double -> Double -> Double -> Color
rgb8      :: Int -> Int -> Int -> Color              -- 0-255 channels
hex       :: String -> Color                         -- "#ff0000", "f00", etc.
lerp      :: Double -> Color -> Color -> Color       -- linear interpolation
withAlpha :: Double -> Color -> Color
toHex     :: Color -> String                         -- "#rrggbb" or "#rrggbbaa"

-- 39 named colors: black, white, red, green, blue, yellow, cyan, magenta,
-- gold, crimson, coral, navy, purple, violet, teal, olive, ...
```

### Element

```haskell
data Element
  = ECircle !Double | ERect !Double !Double | EPath !Path | EGroup ![Element]
  | EFill !Fill !Element | EStroke !Color !Double !Element
  | ETranslate !Double !Double !Element | ERotate !Double !Element
  | EScale !Double !Double !Element | EOpacity !Double !Element
  | EClip !Element !Element | EFilter !FilterKind !Element
  | ...  -- 27 constructors total

instance Semigroup Element  -- EGroup composition
instance Monoid Element     -- mempty = EEmpty
```

### Shape

```haskell
circle         :: Double -> Element                       -- radius
ellipse        :: Double -> Double -> Element             -- rx, ry
rect           :: Double -> Double -> Element             -- width, height
square         :: Double -> Element
roundedRect    :: Double -> Double -> Double -> Double -> Element  -- w, h, rx, ry
line           :: V2 -> V2 -> Element
polygon        :: [V2] -> Element
regularPolygon :: Int -> Double -> Element                -- sides, radius
star           :: Int -> Double -> Double -> Element      -- points, outer, inner
arc            :: Double -> Double -> Double -> Element   -- radius, start, end (radians)
ring           :: Double -> Double -> Element             -- outer, inner radius
```

### Path DSL

```haskell
buildPath :: PathBuilder () -> Path

startAt  :: V2 -> PathBuilder ()
lineTo   :: V2 -> PathBuilder ()
cubicTo  :: V2 -> V2 -> V2 -> PathBuilder ()   -- control1, control2, end
quadTo   :: V2 -> V2 -> PathBuilder ()          -- control, end
arcTo    :: ArcParams -> V2 -> PathBuilder ()
closePath :: PathBuilder ()

polylinePath :: [V2] -> Path   -- open path through points
polygonPath  :: [V2] -> Path   -- closed path through points
```

### Transform & Style

```haskell
translate   :: Double -> Double -> Element -> Element
rotate      :: Double -> Element -> Element              -- degrees
rotateAround :: Double -> V2 -> Element -> Element
scale       :: Double -> Element -> Element              -- uniform
scaleXY     :: Double -> Double -> Element -> Element
skewX       :: Double -> Element -> Element              -- degrees
skewY       :: Double -> Element -> Element

fill        :: Color -> Element -> Element
stroke      :: Color -> Double -> Element -> Element     -- color, width
opacity     :: Double -> Element -> Element
fillNone    :: Element -> Element
clip        :: Element -> Element -> Element             -- clip shape, content
mask        :: Element -> Element -> Element
blur        :: Double -> Element -> Element              -- stdDeviation
dropShadow  :: Double -> Double -> Double -> Color -> Element -> Element
```

### SVG Output

```haskell
render        :: Document -> Text          -- pure serialization
renderElement :: Element -> Text           -- fragment (no <svg> wrapper)
writeSvg      :: FilePath -> Document -> IO ()
```

---

## Example

```haskell
import Data.Function ((&))
import GBVector.Color (black, gold, hex, red, white)
import GBVector.Compose (background, document, group)
import GBVector.Gradient (evenStops, linearGradient)
import GBVector.Path (buildPath, closePath, cubicTo, lineTo, startAt)
import GBVector.SVG (writeSvg)
import GBVector.Shape (circle, rect, star)
import GBVector.Style (fill, fillGradient, fillNone, stroke, withId)
import GBVector.Transform (rotate, scale, translate)
import GBVector.Types (V2 (..))

main :: IO ()
main = writeSvg "example.svg" $
  document 400 400 $
    background 400 400 (hex "#1a1a2e") $
      group
        [ -- Gold star with black outline
          star 5 80 35
            & fill gold
            & stroke black 2
            & translate 200 180

        , -- Gradient circle
          circle 40
            & fillGradient (linearGradient (V2 0 0) (V2 80 80) (evenStops [red, gold]))
            & translate 200 320

        , -- Custom path
          EPath (buildPath $ do
            startAt (V2 50 50)
            lineTo (V2 150 50)
            cubicTo (V2 200 100) (V2 200 200) (V2 150 250)
            lineTo (V2 50 250)
            closePath)
            & fillNone
            & stroke white 1.5
            & translate 100 50
        ]
```

---

## Build & Test

Requires [GHCup](https://www.haskell.org/ghcup/) with GHC >= 9.6.

```bash
cabal build                              # Build library
cabal test                               # Run all tests (151 pure tests)
cabal build --ghc-options="-Werror"      # Warnings as errors
cabal haddock                            # Generate docs
```

---

<p align="center">
  <sub>BSD-3-Clause License · <a href="https://github.com/Gondola-Bros-Entertainment">Gondola Bros Entertainment</a></sub>
</p>
