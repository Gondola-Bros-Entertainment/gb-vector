# Changelog

## 0.1.0.0

Initial release.

### Core Types
- `V2` 2D vector, `Segment` (line/cubic/quad/arc), `Path` (closed/open)
- `Color` type with RGBA in `[0, 1]` — `rgb`, `rgba`, `rgb8`, `hex` constructors
- 30+ named colors, `lerp`, `withAlpha`, `toHex`
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
- `PathBuilder` monad: `startAt`, `lineTo`, `cubicTo`, `quadTo`, `arcTo`, `closePath`
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

### Style
- `fill`, `stroke`, `strokeEx`, `opacity`, `fillNone`, `fillRule`
- `clip`, `mask`, `blur`, `dropShadow`, `withId`, `use`

### Noise
- `perlin2D`, `simplex2D` — deterministic 2D noise
- `fbm` — fractional Brownian motion
- `noisePath`, `noiseClosedPath` — procedural path generation
- `wobblePath`, `jitterPoints` — noise-driven distortion
- `voronoiCells`, `voronoiEdges` — Voronoi diagram generation

### Patterns
- `dotGrid`, `lineGrid`, `crosshatch`, `checker` — tileable pattern generators
- `patternDef`, `PatternConfig` — SVG pattern element construction

### SVG Parsing
- `parseSvg`, `parseElement` — parse SVG text back to `Element` trees
- Supports basic shapes, paths, groups, text, and presentation attributes
- Enables round-trip workflows: render, parse, manipulate, re-export

### Composition
- `group`, `empty`, `document`, `documentWithViewBox`, `background`
- `optimizeElement` — collapse redundant transforms and empty groups

### SVG Output
- `render :: Document -> Text` — pure SVG serialization
- `writeSvg :: FilePath -> Document -> IO ()` — file output
