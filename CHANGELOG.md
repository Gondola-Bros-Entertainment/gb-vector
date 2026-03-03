# Changelog

## 0.1.0.2

### Bug Fixes
- Fix Hackage Haddock coverage: document all positional constructor
  arguments with `-- |` comments. Local `cabal haddock` reported 100% on both
  GHC 9.6.7 and 9.8.4, but Hackage's server-side doc build counts positional
  constructor arguments as separate items requiring documentation.

### Improvements
- Upgrade tested GHC from 9.6.7 to 9.8.4

### Affected Types
- `V2`, `ViewBox`, `Segment`, `Color` — all constructor arguments documented
- `Element` — all 30 constructors' positional arguments documented
- `Fill`, `Gradient`, `FilterKind` — all constructor arguments documented
- `ParseError` — `MalformedTag` and `MalformedPath` arguments documented

## 0.1.0.1

### Bug Fixes
- Fix arc segments silently replaced with straight lines in Boolean ops and PathOps
- Fix arc length measurement using straight-line distance instead of true arc length
- Fix `subpath` potential overflow when `t0` is very close to 1
- Fix test suite leaving temporary SVG files in project root

### Improvements
- `cubicLength` now uses adaptive flattening instead of uniform sampling for better accuracy
- `grad2D` expanded from 4 to 8 gradient directions for better noise isotropy
- 100% Haddock coverage across all 17 modules (was 64%)
- Document Sutherland-Hodgman convexity requirement on `intersection` and `difference`
- Document `union` behavior with non-overlapping polygons
- Document `intersectEpsilon` as absolute tolerance
- 9 new tests (294 total): arc flattening, arc measurement, arc boolean ops, subpath edge case, saturate safety, noise determinism, arcToCubics validation

### Internal
- Add `directory` to test suite dependencies for temp file cleanup

## 0.1.0.0

Initial release.

### Core Types
- `V2` 2D vector, `Segment` (line/cubic/quad/arc), `Path` (closed/open)
- `Color` type with RGBA in `[0, 1]` — `rgb`, `rgba`, `rgb8`, `hex` constructors
- 43 named colors, `lerp`, `withAlpha`, `toHex`
- `hsl`, `hsla` — HSL color construction
- Oklab perceptual color space: `toOklab`, `fromOklab`, `lerpOklab`
- Color adjustments: `lighten`, `darken`, `saturate`, `desaturate`, `invert`

### Element Tree
- `Element` recursive sum type — style and transforms as wrapping constructors
- Compose via function application or left-to-right with `(&)`
- `Semigroup`/`Monoid` instance via `EGroup`

### Shapes
- `circle`, `rect`, `roundedRect`, `ellipse`, `polygon`, `star`
- `line`, `square`, `regularPolygon`, `arc`, `ring`

### Path DSL
- `buildPath` — run a `PathBuilder` monad to produce a `Path`
- `PathBuilder` actions: `startAt`, `lineTo`, `cubicTo`, `quadTo`, `arcTo`, `closePath`
- `polylinePath`, `polygonPath` convenience constructors

### Path Operations
- `reversePath`, `measurePath` — reversal and arc length
- `splitPathAt`, `subpath` — splitting and extraction
- `offsetPath` — parallel curve approximation
- `simplifyPath` — Ramer-Douglas-Peucker simplification

### Boolean Operations
- `union`, `intersection`, `difference`, `xorPaths` — polygon clipping
- `pathToPolygon`, `polygonToPath` — conversion utilities
- `polygonArea`, `pointInPolygon` — polygon analysis

### Transforms
- `translate`, `rotate`, `rotateAround`, `scale`, `scaleXY`, `skewX`, `skewY`
- `Matrix` type with `identity`, `composeMatrix`, `applyMatrix`
- Matrix constructors: `translateM`, `rotateM`, `scaleM`, `scaleXYM`, `skewXM`, `skewYM`

### Gradients
- `linearGradient`, `radialGradient` — gradient constructors
- `stop`, `stopWithOpacity`, `evenStops`, `oklabStops` — gradient stop builders

### Style
- `fill`, `fillColor`, `fillGradient`, `fillNone`, `fillRule`
- `stroke`, `strokeEx`, `dashedStroke`, `defaultStrokeConfig`
- `opacity`, `clip`, `mask`, `blur`, `dropShadow`
- `withId`, `use`, `raw`, `title`, `desc`

### Text
- `text`, `textAt`, `textWithConfig` — text element constructors
- `defaultTextConfig`, `fontSize`, `fontFamily`, `bold`, `italic`, `anchor` — config builders

### Noise
- `perlin2D`, `simplex2D` — deterministic 2D noise
- `fbm` — fractional Brownian motion
- `noisePath`, `noiseClosedPath` — procedural path generation
- `wobblePath`, `jitterPoints` — noise-driven distortion
- `voronoiCells`, `voronoiEdges` — Voronoi diagram generation

### Patterns
- `dotGrid`, `lineGrid`, `crosshatch`, `checker` — tileable pattern generators
- `patternDef`, `PatternConfig`, `defaultPatternConfig` — SVG pattern element construction

### SVG Parsing
- `parseSvg`, `parseElement` — parse SVG text back to `Element` trees
- Supports basic shapes, paths, groups, text, and presentation attributes
- Enables round-trip workflows: render, parse, manipulate, re-export

### Composition
- `group`, `empty`, `document`, `documentWithViewBox`, `background`
- `optimizeElement` — collapse redundant transforms and empty groups

### SVG Output
- `render :: Document -> Text` — pure SVG serialization
- `renderCompact :: Document -> Text` — compact SVG output
- `renderElement :: Element -> Text` — render a fragment without `<svg>` wrapper
- `writeSvg :: FilePath -> Document -> IO ()` — file output
