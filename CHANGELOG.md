# Changelog

## 0.1.0.0

Initial release.

### Core Types
- `V2` 2D vector, `Segment` (line/cubic/quad/arc), `Path` (closed/open)
- `Color` type with RGBA in `[0, 1]` — `rgb`, `rgba`, `rgb8`, `hex` constructors
- 30+ named colors, `lerp`, `withAlpha`, `toHex`

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

### Transforms
- `translate`, `rotate`, `rotateAround`, `scale`, `scaleXY`, `skewX`, `skewY`

### Style
- `fill`, `stroke`, `strokeEx`, `opacity`, `fillNone`, `fillRule`
- `clip`, `mask`, `blur`, `dropShadow`, `withId`, `use`

### Composition
- `group`, `empty`, `document`, `documentWithViewBox`, `background`

### SVG Output
- `render :: Document -> Text` — pure SVG serialization
- `writeSvg :: FilePath -> Document -> IO ()` — file output
