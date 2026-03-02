-- | gb-vector test suite.
--
-- Hand-rolled assertions — first failure stops all. Same pattern as gb-sprite.
module Main (main) where

import Data.Function ((&))
import Data.Maybe (isNothing)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import GBVector.Bezier (cubicBBox, cubicLength, evalCubic, evalQuad, flattenCubic, splitCubicAt, subdivideCubic)
import GBVector.Boolean (intersection, pathToPolygon, pointInPolygon, polygonArea, polygonToPath, union)
import GBVector.Color
  ( Color (..),
    black,
    blue,
    crimson,
    darken,
    desaturate,
    fromOklab,
    gold,
    green,
    hex,
    hsl,
    hsla,
    invert,
    lerp,
    lerpOklab,
    lighten,
    red,
    rgb,
    rgb8,
    rgba,
    saturate,
    toHex,
    toOklab,
    transparent,
    white,
    withAlpha,
  )
import GBVector.Compose (background, document, documentWithViewBox, empty, group, optimizeElement)
import GBVector.Element
  ( Document (..),
    Element (..),
    Fill (..),
    Gradient (..),
    GradientStop (..),
    StrokeConfig (..),
    TextAnchor (..),
    TextConfig (..),
  )
import GBVector.Gradient (evenStops, linearGradient, oklabStops, radialGradient, stop)
import GBVector.Noise (fbm, jitterPoints, noiseClosedPath, noisePath, perlin2D, simplex2D, voronoiCells, voronoiEdges, wobblePath)
import GBVector.Path (buildPath, closePath, cubicTo, lineTo, polygonPath, polylinePath, startAt)
import GBVector.PathOps (measurePath, offsetPath, reversePath, simplifyPath, splitPathAt, subpath)
import GBVector.Pattern (checker, crosshatch, dotGrid, lineGrid)
import GBVector.SVG (render, renderElement, writeSvg)
import GBVector.SVG.Parse (parseElement, parseSvg)
import GBVector.Shape (arc, circle, ellipse, line, polygon, rect, regularPolygon, ring, roundedRect, square, star)
import GBVector.Style (blur, clip, dashedStroke, defaultStrokeConfig, desc, dropShadow, fill, fillNone, opacity, stroke, title, use, withId)
import GBVector.Text (defaultTextConfig, text, textAt)
import GBVector.Transform
  ( Matrix (..),
    applyMatrix,
    composeMatrix,
    identity,
    rotate,
    rotateAround,
    rotateM,
    scale,
    scaleM,
    scaleXY,
    scaleXYM,
    skewX,
    skewXM,
    skewY,
    translate,
    translateM,
  )
import GBVector.Types
  ( FillRule (..),
    LineCap (..),
    LineJoin (..),
    Path (..),
    Segment (..),
    SpreadMethod (..),
    V2 (..),
    ViewBox (..),
  )
import System.Exit (exitFailure, exitSuccess)
import System.IO (hClose, hFlush, openTempFile, stdout)

-- ---------------------------------------------------------------------------
-- Test harness
-- ---------------------------------------------------------------------------

type TestResult = Either String ()

assertEqual :: (Show a, Eq a) => String -> a -> a -> TestResult
assertEqual label expected actual
  | expected == actual = Right ()
  | otherwise =
      Left
        ( label
            ++ ": expected "
            ++ show expected
            ++ ", got "
            ++ show actual
        )

assertTrue :: String -> Bool -> TestResult
assertTrue _ True = Right ()
assertTrue label False = Left (label ++ ": expected True")

assertApprox :: String -> Double -> Double -> Double -> TestResult
assertApprox label tolerance expected actual
  | abs (expected - actual) <= tolerance = Right ()
  | otherwise =
      Left
        ( label
            ++ ": expected ~"
            ++ show expected
            ++ " (±"
            ++ show tolerance
            ++ "), got "
            ++ show actual
        )

assertContains :: String -> Text -> Text -> TestResult
assertContains label needle haystack
  | T.isInfixOf needle haystack = Right ()
  | otherwise =
      Left
        ( label
            ++ ": expected to contain \""
            ++ T.unpack needle
            ++ "\""
        )

runTests :: [(String, TestResult)] -> IO ()
runTests tests = go tests (0 :: Int) (0 :: Int)
  where
    go [] passed total = do
      putStrLn ""
      putStrLn
        ( show passed
            ++ "/"
            ++ show total
            ++ " tests passed."
        )
      if passed == total then exitSuccess else exitFailure
    go ((name, result) : rest) passed total = do
      case result of
        Right () -> do
          putStrLn ("  PASS: " ++ name)
          hFlush stdout
          go rest (passed + 1) (total + 1)
        Left msg -> do
          putStrLn ("  FAIL: " ++ name ++ " - " ++ msg)
          hFlush stdout
          exitFailure

-- ---------------------------------------------------------------------------
-- Main
-- ---------------------------------------------------------------------------

main :: IO ()
main = do
  putStrLn "gb-vector tests"
  putStrLn (replicate 40 '-')
  svgFileTests <- testSvgFile
  runTests
    ( testTypes
        ++ testColor
        ++ testColorHex
        ++ testColorHsl
        ++ testColorOklab
        ++ testColorAdjust
        ++ testElement
        ++ testPath
        ++ testShape
        ++ testTransform
        ++ testMatrix
        ++ testStyle
        ++ testCompose
        ++ testSvgRender
        ++ testBezier
        ++ testGradient
        ++ testText
        ++ testNoise
        ++ testPattern
        ++ testPathOps
        ++ testBoolean
        ++ testParse
        ++ testAccessibility
        ++ testOptimize
        ++ svgFileTests
    )

-- ---------------------------------------------------------------------------
-- Types tests
-- ---------------------------------------------------------------------------

testTypes :: [(String, TestResult)]
testTypes =
  [ ( "V2 equality",
      assertEqual "V2 eq" (V2 1 2) (V2 1 2)
    ),
    ( "V2 inequality",
      assertTrue "V2 neq" (V2 1 2 /= V2 3 4)
    ),
    ( "V2 ordering",
      assertTrue "V2 ord" (V2 0 0 < V2 1 0)
    ),
    ( "Path show roundtrip",
      let path = Path (V2 0 0) [LineTo (V2 10 10)] True
       in assertTrue "path show" (not (null (show path)))
    ),
    ( "LineCap enum coverage",
      assertEqual "cap count" 3 (length [CapButt .. CapSquare])
    ),
    ( "LineJoin enum coverage",
      assertEqual "join count" 3 (length [JoinMiter .. JoinBevel])
    ),
    ( "FillRule enum coverage",
      assertEqual "fill rule count" 2 (length [FillNonZero .. FillEvenOdd])
    ),
    ( "SpreadMethod enum coverage",
      assertEqual "spread count" 3 (length [SpreadPad .. SpreadRepeat])
    ),
    ( "ViewBox show",
      assertTrue "viewbox show" (not (null (show (ViewBox 0 0 100 100))))
    )
  ]

-- ---------------------------------------------------------------------------
-- Color tests
-- ---------------------------------------------------------------------------

testColor :: [(String, TestResult)]
testColor =
  [ ( "rgb clamps above 1",
      assertEqual "rgb clamp high" (Color 1 1 1 1) (rgb 2 3 4)
    ),
    ( "rgb clamps below 0",
      assertEqual "rgb clamp low" (Color 0 0 0 1) (rgb (-1) (-2) (-3))
    ),
    ( "rgba preserves alpha",
      let Color _ _ _ a = rgba 1 0 0 0.5
       in assertApprox "rgba alpha" epsilon 0.5 a
    ),
    ( "rgb8 converts correctly",
      let Color r _ _ _ = rgb8 255 0 0
       in assertApprox "rgb8 red" epsilon 1.0 r
    ),
    ( "rgb8 midpoint",
      let Color r _ _ _ = rgb8 128 0 0
       in assertApprox "rgb8 mid" 0.01 0.502 r
    ),
    ( "lerp t=0 gives start",
      assertEqual "lerp 0" red (lerp 0.0 red blue)
    ),
    ( "lerp t=1 gives end",
      assertEqual "lerp 1" blue (lerp 1.0 red blue)
    ),
    ( "lerp t=0.5 midpoint",
      let Color r _ _ _ = lerp 0.5 black white
       in assertApprox "lerp mid" epsilon 0.5 r
    ),
    ( "lerp clamps t above 1",
      assertEqual "lerp clamp high" blue (lerp 2.0 red blue)
    ),
    ( "lerp clamps t below 0",
      assertEqual "lerp clamp low" red (lerp (-1.0) red blue)
    ),
    ( "withAlpha preserves RGB",
      let Color r g b _ = withAlpha 0.5 red
       in assertTrue "withAlpha RGB" (r == 1 && g == 0 && b == 0)
    ),
    ( "withAlpha sets alpha",
      let Color _ _ _ a = withAlpha 0.3 red
       in assertApprox "withAlpha a" epsilon 0.3 a
    ),
    ( "transparent has zero alpha",
      let Color _ _ _ a = transparent
       in assertApprox "transparent" epsilon 0 a
    ),
    ( "named color gold is correct",
      let Color r g b _ = gold
       in assertTrue "gold rgb" (r == 1 && g > 0.8 && b == 0)
    ),
    ( "named color crimson is correct",
      let Color r g b _ = crimson
       in assertTrue "crimson rgb" (r > 0.85 && g < 0.1 && b > 0.2)
    )
  ]

-- ---------------------------------------------------------------------------
-- Color hex tests
-- ---------------------------------------------------------------------------

testColorHex :: [(String, TestResult)]
testColorHex =
  [ ( "hex parses 6-digit",
      assertEqual "hex 6" (Color 1 0 0 1) (hex "#ff0000")
    ),
    ( "hex parses 3-digit",
      assertEqual "hex 3" (hex "#ff0000") (hex "#f00")
    ),
    ( "hex parses without hash",
      assertEqual "hex no hash" (Color 1 0 0 1) (hex "ff0000")
    ),
    ( "hex parses 8-digit with alpha",
      let Color _ _ _ a = hex "#ff000080"
       in assertApprox "hex alpha" 0.01 0.502 a
    ),
    ( "hex invalid returns black",
      assertEqual "hex invalid" black (hex "xyz")
    ),
    ( "toHex roundtrip red",
      assertEqual "toHex red" "#ff0000" (toHex red)
    ),
    ( "toHex roundtrip white",
      assertEqual "toHex white" "#ffffff" (toHex white)
    ),
    ( "toHex roundtrip black",
      assertEqual "toHex black" "#000000" (toHex black)
    ),
    ( "toHex includes alpha when < 1",
      let result = toHex (rgba 1 0 0 0.5)
       in assertTrue "toHex alpha" (length result == 9)
    )
  ]

-- ---------------------------------------------------------------------------
-- HSL tests
-- ---------------------------------------------------------------------------

testColorHsl :: [(String, TestResult)]
testColorHsl =
  [ ( "hsl red is (0, 1, 0.5)",
      let Color r g b _ = hsl 0 1 0.5
       in do
            _ <- assertApprox "hsl red r" colorTolerance 1.0 r
            _ <- assertApprox "hsl red g" colorTolerance 0.0 g
            assertApprox "hsl red b" colorTolerance 0.0 b
    ),
    ( "hsl green is (120, 1, 0.5)",
      let Color r g b _ = hsl 120 1 0.5
       in do
            _ <- assertApprox "hsl green r" colorTolerance 0.0 r
            _ <- assertApprox "hsl green g" colorTolerance 1.0 g
            assertApprox "hsl green b" colorTolerance 0.0 b
    ),
    ( "hsl blue is (240, 1, 0.5)",
      let Color r g b _ = hsl 240 1 0.5
       in do
            _ <- assertApprox "hsl blue r" colorTolerance 0.0 r
            _ <- assertApprox "hsl blue g" colorTolerance 0.0 g
            assertApprox "hsl blue b" colorTolerance 1.0 b
    ),
    ( "hsl white is (0, 0, 1)",
      let Color r g b _ = hsl 0 0 1
       in do
            _ <- assertApprox "hsl white r" colorTolerance 1.0 r
            _ <- assertApprox "hsl white g" colorTolerance 1.0 g
            assertApprox "hsl white b" colorTolerance 1.0 b
    ),
    ( "hsl black is (0, 0, 0)",
      let Color r g b _ = hsl 0 0 0
       in do
            _ <- assertApprox "hsl black r" colorTolerance 0.0 r
            _ <- assertApprox "hsl black g" colorTolerance 0.0 g
            assertApprox "hsl black b" colorTolerance 0.0 b
    ),
    ( "hsla preserves alpha",
      let Color _ _ _ a = hsla 0 1 0.5 0.7
       in assertApprox "hsla alpha" colorTolerance 0.7 a
    ),
    ( "hsl wraps hue above 360",
      let c1 = hsl 0 1 0.5
          c2 = hsl 360 1 0.5
          Color r1 g1 b1 _ = c1
          Color r2 g2 b2 _ = c2
       in do
            _ <- assertApprox "wrap hue r" colorTolerance r1 r2
            _ <- assertApprox "wrap hue g" colorTolerance g1 g2
            assertApprox "wrap hue b" colorTolerance b1 b2
    ),
    ( "hsl negative hue wraps",
      let c1 = hsl 300 1 0.5
          c2 = hsl (-60) 1 0.5
          Color r1 g1 b1 _ = c1
          Color r2 g2 b2 _ = c2
       in do
            _ <- assertApprox "neg hue r" colorTolerance r1 r2
            _ <- assertApprox "neg hue g" colorTolerance g1 g2
            assertApprox "neg hue b" colorTolerance b1 b2
    ),
    ( "hsl yellow is (60, 1, 0.5)",
      let Color r g b _ = hsl 60 1 0.5
       in do
            _ <- assertApprox "hsl yellow r" colorTolerance 1.0 r
            _ <- assertApprox "hsl yellow g" colorTolerance 1.0 g
            assertApprox "hsl yellow b" colorTolerance 0.0 b
    ),
    ( "hsl cyan is (180, 1, 0.5)",
      let Color r g b _ = hsl 180 1 0.5
       in do
            _ <- assertApprox "hsl cyan r" colorTolerance 0.0 r
            _ <- assertApprox "hsl cyan g" colorTolerance 1.0 g
            assertApprox "hsl cyan b" colorTolerance 1.0 b
    )
  ]

-- ---------------------------------------------------------------------------
-- OKLAB tests
-- ---------------------------------------------------------------------------

testColorOklab :: [(String, TestResult)]
testColorOklab =
  [ ( "toOklab black is (0, 0, 0)",
      let (l, a, b) = toOklab black
       in do
            _ <- assertApprox "oklab black L" colorTolerance 0.0 l
            _ <- assertApprox "oklab black a" colorTolerance 0.0 a
            assertApprox "oklab black b" colorTolerance 0.0 b
    ),
    ( "toOklab white has L near 1",
      let (l, _, _) = toOklab white
       in assertApprox "oklab white L" 0.01 1.0 l
    ),
    ( "fromOklab roundtrip red",
      let (l, a, b) = toOklab red
          Color rr rg rb _ = fromOklab l a b
       in do
            _ <- assertApprox "oklab rt red r" 0.01 1.0 rr
            _ <- assertApprox "oklab rt red g" 0.01 0.0 rg
            assertApprox "oklab rt red b" 0.01 0.0 rb
    ),
    ( "fromOklab roundtrip blue",
      let (l, a, b) = toOklab blue
          Color br bg bb _ = fromOklab l a b
       in do
            _ <- assertApprox "oklab rt blue r" 0.01 0.0 br
            _ <- assertApprox "oklab rt blue g" 0.01 0.0 bg
            assertApprox "oklab rt blue b" 0.01 1.0 bb
    ),
    ( "fromOklab roundtrip gold",
      let (l, a, b) = toOklab gold
          Color gr gg gb _ = fromOklab l a b
          Color er eg eb _ = gold
       in do
            _ <- assertApprox "oklab rt gold r" 0.01 er gr
            _ <- assertApprox "oklab rt gold g" 0.01 eg gg
            assertApprox "oklab rt gold b" 0.01 eb gb
    ),
    ( "lerpOklab t=0 gives start",
      let Color r g b _ = lerpOklab 0.0 red blue
       in do
            _ <- assertApprox "oklab lerp0 r" 0.01 1.0 r
            _ <- assertApprox "oklab lerp0 g" 0.01 0.0 g
            assertApprox "oklab lerp0 b" 0.01 0.0 b
    ),
    ( "lerpOklab t=1 gives end",
      let Color r g b _ = lerpOklab 1.0 red blue
       in do
            _ <- assertApprox "oklab lerp1 r" 0.01 0.0 r
            _ <- assertApprox "oklab lerp1 g" 0.01 0.0 g
            assertApprox "oklab lerp1 b" 0.01 1.0 b
    ),
    ( "lerpOklab midpoint differs from linear lerp",
      let okMid = lerpOklab 0.5 red green
          linMid = lerp 0.5 red green
       in assertTrue "oklab mid differs" (okMid /= linMid)
    ),
    ( "lerpOklab interpolates alpha",
      let Color _ _ _ a = lerpOklab 0.5 (rgba 1 0 0 0) (rgba 0 0 1 1)
       in assertApprox "oklab alpha lerp" colorTolerance 0.5 a
    ),
    ( "toOklab red has positive a (red axis)",
      let (_, a, _) = toOklab red
       in assertTrue "red a > 0" (a > 0)
    ),
    ( "toOklab blue has negative b (blue axis)",
      let (_, _, b) = toOklab blue
       in assertTrue "blue b < 0" (b < 0)
    )
  ]

-- ---------------------------------------------------------------------------
-- Color adjustment tests
-- ---------------------------------------------------------------------------

testColorAdjust :: [(String, TestResult)]
testColorAdjust =
  [ ( "lighten increases lightness",
      let (origL, _, _) = toOklab red
          (newL, _, _) = toOklab (lighten 0.2 red)
       in assertTrue "lighten increases L" (newL > origL)
    ),
    ( "darken decreases lightness",
      let (origL, _, _) = toOklab red
          (newL, _, _) = toOklab (darken 0.2 red)
       in assertTrue "darken decreases L" (newL < origL)
    ),
    ( "lighten 0 is identity",
      let (origL, _, _) = toOklab red
          (newL, _, _) = toOklab (lighten 0 red)
       in assertApprox "lighten 0" 0.01 origL newL
    ),
    ( "darken 0 is identity",
      let (origL, _, _) = toOklab red
          (newL, _, _) = toOklab (darken 0 red)
       in assertApprox "darken 0" 0.01 origL newL
    ),
    ( "lighten preserves alpha",
      let Color _ _ _ a = lighten 0.2 (rgba 1 0 0 0.3)
       in assertApprox "lighten alpha" colorTolerance 0.3 a
    ),
    ( "darken preserves alpha",
      let Color _ _ _ a = darken 0.2 (rgba 1 0 0 0.3)
       in assertApprox "darken alpha" colorTolerance 0.3 a
    ),
    ( "saturate increases chroma",
      let muted = rgb 0.5 0.4 0.3
          (_, oa, ob) = toOklab muted
          origChroma = sqrt (oa * oa + ob * ob)
          (_, na, nb) = toOklab (saturate 0.5 muted)
          newChroma = sqrt (na * na + nb * nb)
       in assertTrue "saturate chroma" (newChroma > origChroma)
    ),
    ( "desaturate decreases chroma",
      let muted = rgb 0.5 0.4 0.3
          (_, oa, ob) = toOklab muted
          origChroma = sqrt (oa * oa + ob * ob)
          (_, na, nb) = toOklab (desaturate 0.5 muted)
          newChroma = sqrt (na * na + nb * nb)
       in assertTrue "desaturate chroma" (newChroma < origChroma)
    ),
    ( "invert black is white",
      assertEqual "invert black" white (invert black)
    ),
    ( "invert white is black",
      assertEqual "invert white" black (invert white)
    ),
    ( "invert preserves alpha",
      let Color _ _ _ a = invert (rgba 1 0 0 0.5)
       in assertApprox "invert alpha" epsilon 0.5 a
    ),
    ( "double invert is identity",
      let original = rgb 0.3 0.6 0.9
          Color r1 g1 b1 _ = original
          Color r2 g2 b2 _ = invert (invert original)
       in do
            _ <- assertApprox "double inv r" epsilon r1 r2
            _ <- assertApprox "double inv g" epsilon g1 g2
            assertApprox "double inv b" epsilon b1 b2
    )
  ]

-- ---------------------------------------------------------------------------
-- Element tests
-- ---------------------------------------------------------------------------

testElement :: [(String, TestResult)]
testElement =
  [ ( "EEmpty is mempty",
      assertEqual "mempty" EEmpty (mempty :: Element)
    ),
    ( "EEmpty <> x = x",
      assertEqual "left identity" (ECircle 5) (EEmpty <> ECircle 5)
    ),
    ( "x <> EEmpty = x",
      assertEqual "right identity" (ECircle 5) (ECircle 5 <> EEmpty)
    ),
    ( "semigroup combines into group",
      case ECircle 5 <> ERect 10 20 of
        EGroup [ECircle 5, ERect 10 20] -> Right ()
        other -> Left ("unexpected: " ++ show other)
    ),
    ( "group flattening",
      case EGroup [ECircle 1] <> EGroup [ECircle 2] of
        EGroup [ECircle 1, ECircle 2] -> Right ()
        other -> Left ("unexpected: " ++ show other)
    ),
    ( "Document fields",
      let doc = Document 100 200 Nothing EEmpty
       in assertTrue "doc fields" (docWidth doc == 100 && docHeight doc == 200)
    ),
    ( "Fill SolidFill equality",
      assertEqual "solid fill" (SolidFill red) (SolidFill red)
    ),
    ( "Fill NoFill equality",
      assertEqual "no fill" NoFill NoFill
    )
  ]

-- ---------------------------------------------------------------------------
-- Path tests
-- ---------------------------------------------------------------------------

testPath :: [(String, TestResult)]
testPath =
  [ ( "buildPath empty",
      let path = buildPath (pure ())
       in assertEqual "empty path" (Path (V2 0 0) [] False) path
    ),
    ( "buildPath with startAt",
      let path = buildPath (startAt (V2 10 20))
       in assertEqual "start" (V2 10 20) (pathStart path)
    ),
    ( "buildPath lineTo",
      let path = buildPath (startAt (V2 0 0) >> lineTo (V2 100 100))
       in assertEqual "lineTo" [LineTo (V2 100 100)] (pathSegments path)
    ),
    ( "buildPath closePath",
      let path = buildPath (startAt (V2 0 0) >> lineTo (V2 100 0) >> closePath)
       in assertTrue "closed" (pathClosed path)
    ),
    ( "buildPath cubicTo",
      let path = buildPath (startAt (V2 0 0) >> cubicTo (V2 10 20) (V2 30 40) (V2 50 50))
       in case pathSegments path of
            [CubicTo (V2 10 20) (V2 30 40) (V2 50 50)] -> Right ()
            other -> Left ("unexpected: " ++ show other)
    ),
    ( "buildPath multiple segments preserve order",
      let path = buildPath $ do
            startAt (V2 0 0)
            lineTo (V2 10 0)
            lineTo (V2 10 10)
            lineTo (V2 0 10)
       in assertEqual "seg count" 3 (length (pathSegments path))
    ),
    ( "polylinePath empty",
      let path = polylinePath []
       in assertTrue "empty polyline" (null (pathSegments path) && not (pathClosed path))
    ),
    ( "polylinePath single point",
      let path = polylinePath [V2 0 0]
       in assertTrue "single polyline" (null (pathSegments path))
    ),
    ( "polylinePath creates open path",
      let path = polylinePath [V2 0 0, V2 10 0, V2 10 10]
       in assertTrue "open polyline" (not (pathClosed path) && length (pathSegments path) == 2)
    ),
    ( "polygonPath creates closed path",
      let path = polygonPath [V2 0 0, V2 10 0, V2 10 10]
       in assertTrue "closed polygon" (pathClosed path && length (pathSegments path) == 2)
    ),
    ( "polygonPath needs 3+ points",
      let path = polygonPath [V2 0 0, V2 10 0]
       in assertTrue "too few" (null (pathSegments path))
    )
  ]

-- ---------------------------------------------------------------------------
-- Shape tests
-- ---------------------------------------------------------------------------

testShape :: [(String, TestResult)]
testShape =
  [ ( "circle creates ECircle",
      assertEqual "circle" (ECircle 25) (circle 25)
    ),
    ( "ellipse creates EEllipse",
      assertEqual "ellipse" (EEllipse 30 20) (ellipse 30 20)
    ),
    ( "rect creates ERect",
      assertEqual "rect" (ERect 100 50) (rect 100 50)
    ),
    ( "square creates equal-sided ERect",
      assertEqual "square" (ERect 40 40) (square 40)
    ),
    ( "roundedRect creates ERoundRect",
      assertEqual "rounded" (ERoundRect 100 50 5 5) (roundedRect 100 50 5 5)
    ),
    ( "line creates ELine",
      assertEqual "line" (ELine (V2 0 0) (V2 100 100)) (line (V2 0 0) (V2 100 100))
    ),
    ( "polygon creates EPolygon",
      let pts = [V2 0 0, V2 10 0, V2 5 10]
       in assertEqual "polygon" (EPolygon pts) (polygon pts)
    ),
    ( "regularPolygon has correct vertex count",
      case regularPolygon 6 50 of
        EPolygon pts -> assertEqual "hex vertices" 6 (length pts)
        other -> Left ("unexpected: " ++ show other)
    ),
    ( "regularPolygon with n<3 is empty",
      case regularPolygon 2 50 of
        EPolygon pts -> assertEqual "empty poly" 0 (length pts)
        other -> Left ("unexpected: " ++ show other)
    ),
    ( "star has correct vertex count",
      case star 5 50 25 of
        EPolygon pts -> assertEqual "star vertices" 10 (length pts)
        other -> Left ("unexpected: " ++ show other)
    ),
    ( "star with n<2 is empty",
      case star 1 50 25 of
        EPolygon pts -> assertEqual "empty star" 0 (length pts)
        other -> Left ("unexpected: " ++ show other)
    ),
    ( "arc creates polyline",
      case arc 50 0 pi of
        EPolyline pts -> assertTrue "arc points" (length pts > 2)
        other -> Left ("unexpected: " ++ show other)
    ),
    ( "ring creates polygon",
      case ring 50 30 of
        EPolygon pts -> assertTrue "ring points" (length pts > 4)
        other -> Left ("unexpected: " ++ show other)
    )
  ]

-- ---------------------------------------------------------------------------
-- Transform tests
-- ---------------------------------------------------------------------------

testTransform :: [(String, TestResult)]
testTransform =
  [ ( "translate wraps element",
      assertEqual "translate" (ETranslate 10 20 (ECircle 5)) (translate 10 20 (circle 5))
    ),
    ( "rotate wraps element",
      assertEqual "rotate" (ERotate 45 (ECircle 5)) (rotate 45 (circle 5))
    ),
    ( "rotateAround wraps element",
      assertEqual "rotateAround" (ERotateAround 90 (V2 50 50) (ECircle 5)) (rotateAround 90 (V2 50 50) (circle 5))
    ),
    ( "scale wraps element uniformly",
      assertEqual "scale" (EScale 2 2 (ECircle 5)) (scale 2 (circle 5))
    ),
    ( "scaleXY wraps element non-uniformly",
      assertEqual "scaleXY" (EScale 2 3 (ECircle 5)) (scaleXY 2 3 (circle 5))
    ),
    ( "skewX wraps element",
      assertEqual "skewX" (ESkewX 15 (ECircle 5)) (skewX 15 (circle 5))
    ),
    ( "skewY wraps element",
      assertEqual "skewY" (ESkewY 15 (ECircle 5)) (skewY 15 (circle 5))
    ),
    ( "compose transforms nest",
      let el = translate 10 20 $ rotate 45 $ circle 5
       in case el of
            ETranslate 10 20 (ERotate 45 (ECircle 5)) -> Right ()
            other -> Left ("unexpected: " ++ show other)
    ),
    ( "(&) composes left to right",
      let el = circle 5 & rotate 45 & translate 10 20
       in case el of
            ETranslate 10 20 (ERotate 45 (ECircle 5)) -> Right ()
            other -> Left ("unexpected: " ++ show other)
    )
  ]

-- ---------------------------------------------------------------------------
-- Matrix tests
-- ---------------------------------------------------------------------------

testMatrix :: [(String, TestResult)]
testMatrix =
  [ ( "identity matrix",
      assertEqual "identity" (Matrix 1 0 0 1 0 0) identity
    ),
    ( "identity apply preserves point",
      assertEqual "id apply" (V2 42 17) (applyMatrix identity (V2 42 17))
    ),
    ( "translateM moves point",
      let m = translateM 10 20
       in assertEqual "translate apply" (V2 15 25) (applyMatrix m (V2 5 5))
    ),
    ( "scaleM scales point",
      let m = scaleM 3
       in assertEqual "scale apply" (V2 15 30) (applyMatrix m (V2 5 10))
    ),
    ( "scaleXYM scales non-uniformly",
      let m = scaleXYM 2 3
       in assertEqual "scaleXY apply" (V2 10 30) (applyMatrix m (V2 5 10))
    ),
    ( "rotateM 90 degrees",
      let m = rotateM 90
          V2 rx ry = applyMatrix m (V2 1 0)
       in do
            _ <- assertApprox "rot90 x" colorTolerance 0.0 rx
            assertApprox "rot90 y" colorTolerance 1.0 ry
    ),
    ( "rotateM 180 degrees",
      let m = rotateM 180
          V2 rx ry = applyMatrix m (V2 1 0)
       in do
            _ <- assertApprox "rot180 x" colorTolerance (-1.0) rx
            assertApprox "rot180 y" colorTolerance 0.0 ry
    ),
    ( "compose identity is identity",
      let m = composeMatrix identity identity
       in assertEqual "id compose" identity m
    ),
    ( "compose translate then scale",
      let m = composeMatrix (scaleM 2) (translateM 5 10)
          V2 rx ry = applyMatrix m (V2 0 0)
       in do
            _ <- assertApprox "ts x" colorTolerance 10.0 rx
            assertApprox "ts y" colorTolerance 20.0 ry
    ),
    ( "compose scale then translate",
      let m = composeMatrix (translateM 5 10) (scaleM 2)
          V2 rx ry = applyMatrix m (V2 3 4)
       in do
            _ <- assertApprox "st x" colorTolerance 11.0 rx
            assertApprox "st y" colorTolerance 18.0 ry
    ),
    ( "skewXM identity at 0",
      let m = skewXM 0
       in assertEqual "skewX 0" (V2 5 10) (applyMatrix m (V2 5 10))
    ),
    ( "skewXM shifts x by y*tan(angle)",
      let m = skewXM 45
          V2 rx ry = applyMatrix m (V2 0 1)
       in do
            _ <- assertApprox "skewX x" colorTolerance 1.0 rx
            assertApprox "skewX y" colorTolerance 1.0 ry
    ),
    ( "rotateM 360 is identity",
      let m = rotateM 360
          V2 rx ry = applyMatrix m (V2 7 13)
       in do
            _ <- assertApprox "rot360 x" colorTolerance 7.0 rx
            assertApprox "rot360 y" colorTolerance 13.0 ry
    )
  ]

-- ---------------------------------------------------------------------------
-- Style tests
-- ---------------------------------------------------------------------------

testStyle :: [(String, TestResult)]
testStyle =
  [ ( "fill wraps with SolidFill",
      case fill red (circle 5) of
        EFill (SolidFill c) (ECircle 5) -> assertEqual "fill color" red c
        other -> Left ("unexpected: " ++ show other)
    ),
    ( "fillNone wraps with NoFill",
      case fillNone (circle 5) of
        EFill NoFill (ECircle 5) -> Right ()
        other -> Left ("unexpected: " ++ show other)
    ),
    ( "stroke wraps element",
      case stroke black 2 (circle 5) of
        EStroke c w (ECircle 5) -> do
          _ <- assertEqual "stroke color" black c
          assertEqual "stroke width" 2.0 w
        other -> Left ("unexpected: " ++ show other)
    ),
    ( "dashedStroke wraps with dash array",
      case dashedStroke black 2 [5, 3] (circle 5) of
        EStrokeEx cfg (ECircle 5) -> do
          _ <- assertEqual "dash color" black (strokeConfigColor cfg)
          _ <- assertEqual "dash width" 2.0 (strokeConfigWidth cfg)
          assertEqual "dash array" [5, 3] (strokeConfigDashArray cfg)
        other -> Left ("unexpected: " ++ show other)
    ),
    ( "defaultStrokeConfig has sane defaults",
      do
        _ <- assertEqual "default width" 1.0 (strokeConfigWidth defaultStrokeConfig)
        assertEqual "default dash" [] (strokeConfigDashArray defaultStrokeConfig)
    ),
    ( "render dashedStroke includes stroke-dasharray",
      let svg = renderElement (dashedStroke black 2 [5, 3] (circle 5))
       in assertContains "dasharray" (T.pack "stroke-dasharray") svg
    ),
    ( "opacity wraps element",
      assertEqual "opacity" (EOpacity 0.5 (ECircle 5)) (opacity 0.5 (circle 5))
    ),
    ( "clip wraps element",
      case clip (rect 100 100) (circle 50) of
        EClip (ERect 100 100) (ECircle 50) -> Right ()
        other -> Left ("unexpected: " ++ show other)
    ),
    ( "blur wraps element",
      case blur 5 (circle 50) of
        EFilter _ (ECircle 50) -> Right ()
        other -> Left ("unexpected: " ++ show other)
    ),
    ( "dropShadow wraps element",
      case dropShadow 2 2 4 black (circle 50) of
        EFilter _ (ECircle 50) -> Right ()
        other -> Left ("unexpected: " ++ show other)
    ),
    ( "withId wraps element",
      assertEqual "withId" (EWithId (T.pack "my-circle") (ECircle 5)) (withId (T.pack "my-circle") (circle 5))
    ),
    ( "use creates EUse",
      assertEqual "use" (EUse (T.pack "my-circle")) (use (T.pack "my-circle"))
    ),
    ( "style composition",
      let el = fill red $ stroke black 2 $ circle 30
       in case el of
            EFill (SolidFill _) (EStroke _ _ (ECircle 30)) -> Right ()
            other -> Left ("unexpected: " ++ show other)
    )
  ]

-- ---------------------------------------------------------------------------
-- Compose tests
-- ---------------------------------------------------------------------------

testCompose :: [(String, TestResult)]
testCompose =
  [ ( "group empty list",
      assertEqual "empty group" EEmpty (group [])
    ),
    ( "group single element",
      assertEqual "single group" (ECircle 5) (group [ECircle 5])
    ),
    ( "group multiple elements",
      case group [ECircle 5, ERect 10 20] of
        EGroup [ECircle 5, ERect 10 20] -> Right ()
        other -> Left ("unexpected: " ++ show other)
    ),
    ( "empty is EEmpty",
      assertEqual "empty" EEmpty empty
    ),
    ( "document creates Document",
      let doc = document 100 200 (circle 5)
       in assertTrue "doc" (docWidth doc == 100 && docHeight doc == 200 && isNothing (docViewBox doc))
    ),
    ( "documentWithViewBox creates Document with viewBox",
      let vb = ViewBox 0 0 100 100
          doc = documentWithViewBox 200 200 vb (circle 5)
       in assertEqual "viewBox" (Just vb) (docViewBox doc)
    ),
    ( "background adds rect behind element",
      case background 100 100 white (circle 5) of
        EGroup [EFill (SolidFill _) (ERect 100 100), ECircle 5] -> Right ()
        other -> Left ("unexpected: " ++ show other)
    )
  ]

-- ---------------------------------------------------------------------------
-- SVG render tests
-- ---------------------------------------------------------------------------

testSvgRender :: [(String, TestResult)]
testSvgRender =
  [ ( "render includes svg tag",
      let svg = render (document 100 100 (circle 5))
       in assertContains "svg open" (T.pack "<svg") svg
    ),
    ( "render includes closing svg tag",
      let svg = render (document 100 100 (circle 5))
       in assertContains "svg close" (T.pack "</svg>") svg
    ),
    ( "render includes width",
      let svg = render (document 200 100 (circle 5))
       in assertContains "width" (T.pack "width=\"200\"") svg
    ),
    ( "render includes height",
      let svg = render (document 100 300 (circle 5))
       in assertContains "height" (T.pack "height=\"300\"") svg
    ),
    ( "render includes viewBox",
      let svg = render (documentWithViewBox 100 100 (ViewBox 0 0 50 50) (circle 5))
       in assertContains "viewBox" (T.pack "viewBox=\"0 0 50 50\"") svg
    ),
    ( "render circle",
      let svg = renderElement (circle 25)
       in assertContains "circle" (T.pack "<circle") svg
    ),
    ( "render rect",
      let svg = renderElement (rect 100 50)
       in assertContains "rect" (T.pack "<rect") svg
    ),
    ( "render ellipse",
      let svg = renderElement (ellipse 30 20)
       in assertContains "ellipse" (T.pack "<ellipse") svg
    ),
    ( "render line",
      let svg = renderElement (line (V2 0 0) (V2 100 100))
       in assertContains "line" (T.pack "<line") svg
    ),
    ( "render polygon",
      let svg = renderElement (polygon [V2 0 0, V2 50 0, V2 25 50])
       in assertContains "polygon" (T.pack "<polygon") svg
    ),
    ( "render translate transform",
      let svg = renderElement (translate 10 20 (circle 5))
       in assertContains "translate" (T.pack "translate(10,20)") svg
    ),
    ( "render rotate transform",
      let svg = renderElement (rotate 45 (circle 5))
       in assertContains "rotate" (T.pack "rotate(45)") svg
    ),
    ( "render scale transform",
      let svg = renderElement (scale 2 (circle 5))
       in assertContains "scale" (T.pack "scale(2,2)") svg
    ),
    ( "render fill attribute",
      let svg = renderElement (fill red (circle 5))
       in assertContains "fill" (T.pack "fill=\"#ff0000\"") svg
    ),
    ( "render stroke attribute",
      let svg = renderElement (stroke black 2 (circle 5))
       in assertContains "stroke" (T.pack "stroke=\"#000000\"") svg
    ),
    ( "render opacity attribute",
      let svg = renderElement (opacity 0.5 (circle 5))
       in assertContains "opacity" (T.pack "opacity=\"0.5\"") svg
    ),
    ( "render group",
      let svg = renderElement (group [circle 5, rect 10 20])
       in assertContains "group" (T.pack "<g>") svg
    ),
    ( "render path with lineTo",
      let path = buildPath $ do
            startAt (V2 0 0)
            lineTo (V2 100 100)
          svg = renderElement (EPath path)
       in assertContains "path" (T.pack "<path") svg
    ),
    ( "render closed path has Z",
      let path = buildPath $ do
            startAt (V2 0 0)
            lineTo (V2 100 0)
            lineTo (V2 100 100)
            closePath
          svg = renderElement (EPath path)
       in assertContains "close" (T.pack "Z") svg
    ),
    ( "render text element",
      let svg = renderElement (text (T.pack "Hello"))
       in assertContains "text" (T.pack "<text") svg
    ),
    ( "render text escapes XML",
      let svg = renderElement (text (T.pack "<script>"))
       in assertContains "escaped" (T.pack "&lt;script&gt;") svg
    ),
    ( "render use element",
      let svg = renderElement (use (T.pack "my-id"))
       in assertContains "use" (T.pack "<use") svg
    ),
    ( "render EEmpty produces nothing",
      let svg = renderElement EEmpty
       in assertEqual "empty svg" T.empty svg
    ),
    ( "render filter creates defs",
      let svg = renderElement (blur 5 (circle 50))
       in assertContains "defs" (T.pack "<defs>") svg
    ),
    ( "render clip creates clipPath def",
      let svg = renderElement (clip (rect 100 100) (circle 50))
       in assertContains "clipPath" (T.pack "<clipPath") svg
    ),
    ( "render fillNone",
      let svg = renderElement (fillNone (circle 5))
       in assertContains "fill none" (T.pack "fill=\"none\"") svg
    ),
    ( "render roundedRect",
      let svg = renderElement (roundedRect 100 50 5 10)
       in do
            _ <- assertContains "rx" (T.pack "rx=\"5\"") svg
            assertContains "ry" (T.pack "ry=\"10\"") svg
    ),
    ( "render skewX",
      let svg = renderElement (skewX 15 (circle 5))
       in assertContains "skewX" (T.pack "skewX(15)") svg
    ),
    ( "render skewY",
      let svg = renderElement (skewY 20 (circle 5))
       in assertContains "skewY" (T.pack "skewY(20)") svg
    ),
    ( "render rotateAround",
      let svg = renderElement (rotateAround 90 (V2 50 50) (circle 5))
       in assertContains "rotate around" (T.pack "rotate(90,50,50)") svg
    ),
    ( "render withId",
      let svg = renderElement (withId (T.pack "test-id") (circle 5))
       in assertContains "id" (T.pack "id=\"test-id\"") svg
    ),
    ( "numbers strip trailing zeros",
      let svg = renderElement (translate 10.50 20.0 (circle 5))
       in assertContains "clean number" (T.pack "translate(10.5,20)") svg
    )
  ]

-- ---------------------------------------------------------------------------
-- Bezier tests
-- ---------------------------------------------------------------------------

testBezier :: [(String, TestResult)]
testBezier =
  [ ( "evalCubic at t=0 is start",
      let result = evalCubic (V2 0 0) (V2 10 20) (V2 30 40) (V2 50 50) 0
       in assertEqual "cubic t=0" (V2 0 0) result
    ),
    ( "evalCubic at t=1 is end",
      let result = evalCubic (V2 0 0) (V2 10 20) (V2 30 40) (V2 50 50) 1
       in assertEqual "cubic t=1" (V2 50 50) result
    ),
    ( "evalQuad at t=0 is start",
      let result = evalQuad (V2 0 0) (V2 25 50) (V2 50 0) 0
       in assertEqual "quad t=0" (V2 0 0) result
    ),
    ( "evalQuad at t=1 is end",
      let result = evalQuad (V2 0 0) (V2 25 50) (V2 50 0) 1
       in assertEqual "quad t=1" (V2 50 0) result
    ),
    ( "splitCubicAt preserves endpoints",
      let ((left0, _, _, _), (_, _, _, right3)) =
            splitCubicAt (V2 0 0) (V2 10 20) (V2 30 40) (V2 50 50) 0.5
       in do
            _ <- assertEqual "split left start" (V2 0 0) left0
            assertEqual "split right end" (V2 50 50) right3
    ),
    ( "splitCubicAt midpoint matches eval",
      let p0 = V2 0 0
          p1 = V2 10 20
          p2 = V2 30 40
          p3 = V2 50 50
          ((_, _, _, mid), _) = splitCubicAt p0 p1 p2 p3 0.5
          evalMid = evalCubic p0 p1 p2 p3 0.5
          V2 mx my = mid
          V2 ex ey = evalMid
       in do
            _ <- assertApprox "split mid x" epsilon ex mx
            assertApprox "split mid y" epsilon ey my
    ),
    ( "subdivideCubic is splitAt 0.5",
      let p0 = V2 0 0
          p1 = V2 10 20
          p2 = V2 30 40
          p3 = V2 50 50
       in assertEqual "subdivide" (splitCubicAt p0 p1 p2 p3 0.5) (subdivideCubic p0 p1 p2 p3)
    ),
    ( "cubicBBox contains endpoints",
      let p0 = V2 0 0
          p3 = V2 100 100
          (V2 minX minY, V2 maxX maxY) = cubicBBox p0 (V2 25 75) (V2 75 25) p3
       in assertTrue "bbox contains" (minX <= 0 && minY <= 0 && maxX >= 100 && maxY >= 100)
    ),
    ( "flattenCubic returns endpoint",
      let pts = flattenCubic (V2 0 0) (V2 0 100) (V2 100 100) (V2 100 0) 1.0
       in case reverse pts of
            (V2 x y : _) -> do
              _ <- assertApprox "flatten end x" epsilon 100 x
              assertApprox "flatten end y" epsilon 0 y
            [] -> Left "empty flatten result"
    ),
    ( "flattenCubic straight line is short",
      let pts = flattenCubic (V2 0 0) (V2 33 33) (V2 66 66) (V2 100 100) 1.0
       in assertTrue "straight flatten" (length pts <= 4)
    ),
    ( "cubicLength straight line",
      let len = cubicLength (V2 0 0) (V2 33 33) (V2 66 66) (V2 100 100)
          expected = sqrt (100 * 100 + 100 * 100)
       in assertApprox "straight length" 1.0 expected len
    ),
    ( "cubicLength positive",
      let len = cubicLength (V2 0 0) (V2 0 100) (V2 100 100) (V2 100 0)
       in assertTrue "positive length" (len > 0)
    )
  ]

-- ---------------------------------------------------------------------------
-- Gradient tests
-- ---------------------------------------------------------------------------

testGradient :: [(String, TestResult)]
testGradient =
  [ ( "linearGradient creates correct type",
      case linearGradient (V2 0 0) (V2 100 0) [stop 0 red, stop 1 blue] of
        LinearGradient {} -> Right ()
        other -> Left ("unexpected: " ++ show other)
    ),
    ( "radialGradient creates correct type",
      case radialGradient (V2 50 50) 50 [stop 0 white, stop 1 black] of
        RadialGradient {} -> Right ()
        other -> Left ("unexpected: " ++ show other)
    ),
    ( "stop sets offset and color",
      let s = stop 0.5 red
       in do
            _ <- assertApprox "stop offset" epsilon 0.5 (stopOffset s)
            _ <- assertEqual "stop color" red (stopColor s)
            assertApprox "stop opacity" epsilon 1.0 (stopOpacity s)
    ),
    ( "evenStops single color",
      case evenStops [red] of
        [s] -> assertApprox "single offset" epsilon 0.0 (stopOffset s)
        other -> Left ("unexpected count: " ++ show (length other))
    ),
    ( "evenStops two colors",
      case evenStops [red, blue] of
        [s1, s2] -> do
          _ <- assertApprox "first offset" epsilon 0.0 (stopOffset s1)
          assertApprox "second offset" epsilon 1.0 (stopOffset s2)
        other -> Left ("unexpected count: " ++ show (length other))
    ),
    ( "evenStops three colors",
      case evenStops [red, green, blue] of
        [_, s2, _] -> assertApprox "mid offset" epsilon 0.5 (stopOffset s2)
        other -> Left ("unexpected count: " ++ show (length other))
    ),
    ( "evenStops empty",
      assertEqual "empty stops" ([] :: [GradientStop]) (evenStops [])
    ),
    ( "oklabStops generates correct count",
      assertEqual "oklab stops count" 5 (length (oklabStops 5 red blue))
    ),
    ( "oklabStops endpoints match",
      let stops = oklabStops 3 red blue
          Color fr fg fb _ = stopColor (head stops)
          Color lr lg lb _ = stopColor (last stops)
       in do
            _ <- assertApprox "oklab first r" 0.01 1.0 fr
            _ <- assertApprox "oklab first g" 0.01 0.0 fg
            _ <- assertApprox "oklab first b" 0.01 0.0 fb
            _ <- assertApprox "oklab last r" 0.01 0.0 lr
            _ <- assertApprox "oklab last g" 0.01 0.0 lg
            assertApprox "oklab last b" 0.01 1.0 lb
    ),
    ( "oklabStops midpoint differs from linear",
      let oklabMid = stopColor (oklabStops 3 red green !! 1)
          linearMid = stopColor (evenStops [red, lerp 0.5 red green, green] !! 1)
       in assertTrue "oklab mid differs" (oklabMid /= linearMid)
    ),
    ( "render gradient creates defs",
      let grad = linearGradient (V2 0 0) (V2 100 0) [stop 0 red, stop 1 blue]
          svg = renderElement (EFill (GradientFill grad) (rect 100 100))
       in assertContains "gradient def" (T.pack "<linearGradient") svg
    )
  ]

-- ---------------------------------------------------------------------------
-- Text tests
-- ---------------------------------------------------------------------------

testText :: [(String, TestResult)]
testText =
  [ ( "text creates EText",
      case text (T.pack "Hello") of
        EText _ content -> assertEqual "text content" (T.pack "Hello") content
        other -> Left ("unexpected: " ++ show other)
    ),
    ( "textAt positions text",
      case textAt 10 20 (T.pack "Hi") of
        ETranslate 10 20 (EText _ _) -> Right ()
        other -> Left ("unexpected: " ++ show other)
    ),
    ( "defaultTextConfig has sane defaults",
      assertTrue "defaults" (textConfigFontSize defaultTextConfig == 16 && textConfigAnchor defaultTextConfig == AnchorStart)
    ),
    ( "render text includes font-family",
      let svg = renderElement (text (T.pack "Test"))
       in assertContains "font-family" (T.pack "font-family") svg
    ),
    ( "render text includes font-size",
      let svg = renderElement (text (T.pack "Test"))
       in assertContains "font-size" (T.pack "font-size") svg
    )
  ]

-- ---------------------------------------------------------------------------
-- Noise tests
-- ---------------------------------------------------------------------------

testNoise :: [(String, TestResult)]
testNoise =
  [ ( "perlin2D is deterministic",
      let v1 = perlin2D 42 1.5 2.5
          v2 = perlin2D 42 1.5 2.5
       in assertEqual "perlin deterministic" v1 v2
    ),
    ( "perlin2D different seeds differ",
      let v1 = perlin2D 0 5.5 7.3
          v2 = perlin2D 12345 5.5 7.3
       in assertTrue "perlin seeds" (v1 /= v2)
    ),
    ( "perlin2D in approximate range",
      let v = perlin2D 0 3.7 8.2
       in assertTrue "perlin range" (v >= -2.0 && v <= 2.0)
    ),
    ( "simplex2D is deterministic",
      let v1 = simplex2D 42 1.5 2.5
          v2 = simplex2D 42 1.5 2.5
       in assertEqual "simplex deterministic" v1 v2
    ),
    ( "simplex2D different coords differ",
      let v1 = simplex2D 42 1.7 3.2
          v2 = simplex2D 42 8.4 6.1
       in assertTrue "simplex coords" (v1 /= v2)
    ),
    ( "fbm returns finite value",
      let v = fbm (perlin2D 0) 4 2.0 0.5 5.0 5.0
       in assertTrue "fbm finite" (not (isNaN v) && not (isInfinite v))
    ),
    ( "fbm 1 octave equals base noise normalized",
      let v = fbm (perlin2D 0) 1 2.0 0.5 3.0 4.0
          base = perlin2D 0 3.0 4.0
       in assertApprox "fbm 1 octave" epsilon base v
    ),
    ( "noisePath has correct segment count",
      let path = noisePath (perlin2D 0) 10 100 50 0
       in assertEqual "noisePath segs" 9 (length (pathSegments path))
    ),
    ( "noisePath is open",
      let path = noisePath (perlin2D 0) 10 100 50 0
       in assertTrue "noisePath open" (not (pathClosed path))
    ),
    ( "noiseClosedPath is closed",
      let path = noiseClosedPath (perlin2D 0) 20 50 50 30 5
       in assertTrue "noiseClosedPath closed" (pathClosed path)
    ),
    ( "noiseClosedPath has correct segment count",
      let path = noiseClosedPath (perlin2D 0) 20 50 50 30 5
       in assertEqual "noiseClosedPath segs" 19 (length (pathSegments path))
    ),
    ( "wobblePath preserves segment count",
      let original = polylinePath [V2 0 0, V2 50 0, V2 100 0, V2 100 50]
          wobbled = wobblePath (perlin2D 0) 5 original
       in assertEqual "wobble segs" (length (pathSegments original)) (length (pathSegments wobbled))
    ),
    ( "wobblePath modifies points",
      let original = polylinePath [V2 3.7 8.2, V2 50.3 12.1, V2 97.6 43.8]
          wobbled = wobblePath (perlin2D 0) 10 original
       in assertTrue "wobble changes" (pathStart original /= pathStart wobbled)
    ),
    ( "jitterPoints preserves count",
      let pts = [V2 0 0, V2 10 10, V2 20 20]
          jittered = jitterPoints (perlin2D 0) 5 pts
       in assertEqual "jitter count" 3 (length jittered)
    ),
    ( "jitterPoints modifies points",
      let pts = [V2 7.3 4.1, V2 22.8 15.6, V2 38.2 29.7]
          jittered = jitterPoints (perlin2D 0) 10 pts
       in assertTrue "jitter changes" (pts /= jittered)
    ),
    ( "voronoiCells correct count",
      let cells = voronoiCells 42 4 3 100 100
       in assertEqual "voronoi count" 12 (length cells)
    ),
    ( "voronoiCells within bounds",
      let cells = voronoiCells 42 5 5 200 200
       in assertTrue
            "voronoi bounds"
            ( all
                (\(V2 vx vy) -> vx >= -50 && vx <= 250 && vy >= -50 && vy <= 250)
                cells
            )
    ),
    ( "voronoiEdges produces edges",
      let edges = voronoiEdges 42 3 3 100 100
       in assertTrue "voronoi edges" (not (null edges))
    ),
    ( "voronoiEdges correct count",
      -- 3x3 grid: horizontal edges = 3*2 = 6, vertical edges = 2*3 = 6, total = 12
      let edges = voronoiEdges 42 3 3 100 100
       in assertEqual "voronoi edge count" 12 (length edges)
    )
  ]

-- ---------------------------------------------------------------------------
-- Pattern tests
-- ---------------------------------------------------------------------------

testPattern :: [(String, TestResult)]
testPattern =
  [ ( "dotGrid produces group",
      case dotGrid 10 2 red of
        EGroup dots -> assertTrue "dotGrid non-empty" (not (null dots))
        other -> Left ("unexpected: " ++ show other)
    ),
    ( "dotGrid dots are circles",
      case dotGrid 20 3 blue of
        EGroup (first : _) -> case first of
          ETranslate _ _ (EFill _ (ECircle r)) -> assertApprox "dot radius" epsilon 3 r
          other -> Left ("unexpected dot: " ++ show other)
        other -> Left ("unexpected: " ++ show other)
    ),
    ( "lineGrid produces group",
      case lineGrid 15 1 black of
        EGroup elems -> assertTrue "lineGrid non-empty" (not (null elems))
        other -> Left ("unexpected: " ++ show other)
    ),
    ( "crosshatch produces group",
      case crosshatch 10 0.5 black of
        EGroup elems -> assertTrue "crosshatch non-empty" (not (null elems))
        other -> Left ("unexpected: " ++ show other)
    ),
    ( "checker produces group",
      case checker 25 black white of
        EGroup cells -> assertTrue "checker non-empty" (not (null cells))
        other -> Left ("unexpected: " ++ show other)
    ),
    ( "checker cell count matches grid",
      case checker 50 black white of
        EGroup cells -> assertEqual "checker cells" 4 (length cells)
        other -> Left ("unexpected: " ++ show other)
    )
  ]

-- ---------------------------------------------------------------------------
-- PathOps tests
-- ---------------------------------------------------------------------------

-- | A simple square path for testing.
squarePath :: Path
squarePath = polygonPath [V2 0 0, V2 100 0, V2 100 100, V2 0 100]

-- | A simple triangle path for testing.
trianglePath :: Path
trianglePath = polygonPath [V2 0 0, V2 100 0, V2 50 100]

testPathOps :: [(String, TestResult)]
testPathOps =
  [ ( "reversePath preserves closure",
      assertTrue "reverse closed" (pathClosed (reversePath squarePath))
    ),
    ( "reversePath preserves segment count",
      assertEqual "reverse segs" (length (pathSegments squarePath)) (length (pathSegments (reversePath squarePath)))
    ),
    ( "reversePath changes start point",
      assertTrue "reverse start" (pathStart squarePath /= pathStart (reversePath squarePath))
    ),
    ( "measurePath positive for non-empty path",
      let len = measurePath squarePath
       in assertTrue "measure positive" (len > 0)
    ),
    ( "measurePath square is ~400",
      let len = measurePath squarePath
       in assertApprox "measure square" 1.0 400.0 len
    ),
    ( "measurePath triangle is ~300",
      -- Triangle: 100 + 100 + ~112 = ~312 (right triangle with legs 100, hypotenuse ~112)
      let len = measurePath trianglePath
       in assertTrue "measure triangle" (len > 200 && len < 400)
    ),
    ( "splitPathAt 0 gives empty first half",
      let (before, _) = splitPathAt 0 squarePath
       in assertTrue "split 0 before" (null (pathSegments before))
    ),
    ( "splitPathAt 1 gives empty second half",
      let (_, after) = splitPathAt 1 squarePath
       in assertTrue "split 1 after" (null (pathSegments after))
    ),
    ( "splitPathAt 0.5 produces two paths",
      let (before, after) = splitPathAt 0.5 squarePath
       in assertTrue "split 0.5" (not (null (pathSegments before)) && not (null (pathSegments after)))
    ),
    ( "subpath extracts section",
      let sub = subpath 0.25 0.75 squarePath
       in assertTrue "subpath non-empty" (not (null (pathSegments sub)))
    ),
    ( "offsetPath preserves segment structure",
      let offset = offsetPath 5 squarePath
       in assertTrue "offset non-empty" (not (null (pathSegments offset)))
    ),
    ( "offsetPath changes perimeter",
      let original = measurePath squarePath
          offset = measurePath (offsetPath 10 squarePath)
       in assertTrue "offset changes" (abs (offset - original) > 1.0)
    ),
    ( "simplifyPath reduces points on straight line",
      let straightish = polylinePath [V2 0 0, V2 10 0.001, V2 20 0, V2 30 0.001, V2 40 0]
          simplified = simplifyPath 1 straightish
       in assertTrue "simplify reduces" (length (pathSegments simplified) < length (pathSegments straightish))
    ),
    ( "simplifyPath preserves endpoints",
      let path = polylinePath [V2 0 0, V2 50 1, V2 100 0]
          simplified = simplifyPath 2 path
       in assertEqual "simplify start" (V2 0 0) (pathStart simplified)
    )
  ]

-- ---------------------------------------------------------------------------
-- Boolean tests
-- ---------------------------------------------------------------------------

-- | Two overlapping squares for boolean tests.
squareA :: Path
squareA = polygonPath [V2 0 0, V2 100 0, V2 100 100, V2 0 100]

squareB :: Path
squareB = polygonPath [V2 50 50, V2 150 50, V2 150 150, V2 50 150]

testBoolean :: [(String, TestResult)]
testBoolean =
  [ ( "pathToPolygon includes all vertices",
      let poly = pathToPolygon squarePath
       in assertTrue "polygon verts" (length poly >= 4)
    ),
    ( "polygonToPath creates closed path",
      let path = polygonToPath [V2 0 0, V2 10 0, V2 10 10]
       in assertTrue "polygon closed" (pathClosed path)
    ),
    ( "polygonArea of unit square",
      let area = polygonArea [V2 0 0, V2 1 0, V2 1 1, V2 0 1]
       in assertApprox "unit area" 0.01 1.0 area
    ),
    ( "polygonArea of 100x100 square",
      let area = polygonArea [V2 0 0, V2 100 0, V2 100 100, V2 0 100]
       in assertApprox "100x100 area" 1.0 10000.0 area
    ),
    ( "polygonArea clockwise is negative",
      let area = polygonArea [V2 0 0, V2 0 1, V2 1 1, V2 1 0]
       in assertTrue "cw area neg" (area < 0)
    ),
    ( "pointInPolygon center",
      let poly = [V2 0 0, V2 100 0, V2 100 100, V2 0 100]
       in assertTrue "center inside" (pointInPolygon (V2 50 50) poly)
    ),
    ( "pointInPolygon outside",
      let poly = [V2 0 0, V2 100 0, V2 100 100, V2 0 100]
       in assertTrue "outside" (not (pointInPolygon (V2 200 200) poly))
    ),
    ( "intersection of overlapping squares is non-empty",
      let result = intersection squareA squareB
       in assertTrue "intersection non-empty" (not (null (pathSegments result)))
    ),
    ( "intersection is closed",
      let result = intersection squareA squareB
       in assertTrue "intersection closed" (pathClosed result)
    ),
    ( "union of overlapping squares is non-empty",
      let result = union squareA squareB
       in assertTrue "union non-empty" (not (null (pathSegments result)))
    ),
    ( "union is closed",
      let result = union squareA squareB
       in assertTrue "union closed" (pathClosed result)
    )
  ]

-- ---------------------------------------------------------------------------
-- Parse tests
-- ---------------------------------------------------------------------------

testParse :: [(String, TestResult)]
testParse =
  [ ( "parseElement empty is EEmpty",
      assertEqual "parse empty" (Right EEmpty) (parseElement (T.pack ""))
    ),
    ( "parseElement circle",
      case parseElement (T.pack "<circle r=\"50\"/>") of
        Right (ECircle r) -> assertApprox "circle r" epsilon 50 r
        other -> Left ("unexpected: " ++ show other)
    ),
    ( "parseElement circle with cx cy",
      case parseElement (T.pack "<circle cx=\"10\" cy=\"20\" r=\"5\"/>") of
        Right (ETranslate tx ty (ECircle r)) -> do
          _ <- assertApprox "cx" epsilon 10 tx
          _ <- assertApprox "cy" epsilon 20 ty
          assertApprox "r" epsilon 5 r
        other -> Left ("unexpected: " ++ show other)
    ),
    ( "parseElement rect",
      case parseElement (T.pack "<rect width=\"100\" height=\"50\"/>") of
        Right (ERect w h) -> do
          _ <- assertApprox "w" epsilon 100 w
          assertApprox "h" epsilon 50 h
        other -> Left ("unexpected: " ++ show other)
    ),
    ( "parseElement rounded rect",
      case parseElement (T.pack "<rect width=\"100\" height=\"50\" rx=\"10\" ry=\"5\"/>") of
        Right (ERoundRect w h rx ry) -> do
          _ <- assertApprox "w" epsilon 100 w
          _ <- assertApprox "h" epsilon 50 h
          _ <- assertApprox "rx" epsilon 10 rx
          assertApprox "ry" epsilon 5 ry
        other -> Left ("unexpected: " ++ show other)
    ),
    ( "parseElement line",
      case parseElement (T.pack "<line x1=\"0\" y1=\"0\" x2=\"100\" y2=\"50\"/>") of
        Right (ELine (V2 x1 y1) (V2 x2 y2)) -> do
          _ <- assertApprox "x1" epsilon 0 x1
          _ <- assertApprox "y1" epsilon 0 y1
          _ <- assertApprox "x2" epsilon 100 x2
          assertApprox "y2" epsilon 50 y2
        other -> Left ("unexpected: " ++ show other)
    ),
    ( "parseElement path with line",
      case parseElement (T.pack "<path d=\"M10 20 L30 40\"/>") of
        Right (EPath (Path (V2 sx sy) segs False)) -> do
          _ <- assertApprox "start x" epsilon 10 sx
          _ <- assertApprox "start y" epsilon 20 sy
          assertEqual "seg count" 1 (length segs)
        other -> Left ("unexpected: " ++ show other)
    ),
    ( "parseElement closed path",
      case parseElement (T.pack "<path d=\"M0 0 L100 0 L100 100 Z\"/>") of
        Right (EPath (Path _ segs True)) ->
          assertEqual "seg count" 2 (length segs)
        other -> Left ("unexpected: " ++ show other)
    ),
    ( "parseElement path with cubic",
      case parseElement (T.pack "<path d=\"M0 0 C10 20 30 40 50 50\"/>") of
        Right (EPath (Path _ segs False)) ->
          case segs of
            [CubicTo {}] -> Right ()
            _ -> Left ("unexpected segs: " ++ show segs)
        other -> Left ("unexpected: " ++ show other)
    ),
    ( "parseElement group",
      case parseElement (T.pack "<g><circle r=\"5\"/><rect width=\"10\" height=\"10\"/></g>") of
        Right (EGroup children) ->
          assertEqual "group children" 2 (length children)
        other -> Left ("unexpected: " ++ show other)
    ),
    ( "parseElement fill attribute",
      case parseElement (T.pack "<circle r=\"5\" fill=\"#ff0000\"/>") of
        Right (EFill (SolidFill (Color r g b _)) (ECircle _)) -> do
          _ <- assertApprox "fill r" 0.01 1.0 r
          _ <- assertApprox "fill g" 0.01 0.0 g
          assertApprox "fill b" 0.01 0.0 b
        other -> Left ("unexpected: " ++ show other)
    ),
    ( "parseElement fill none",
      case parseElement (T.pack "<circle r=\"5\" fill=\"none\"/>") of
        Right (EFill NoFill (ECircle _)) -> Right ()
        other -> Left ("unexpected: " ++ show other)
    ),
    ( "parseElement transform translate",
      case parseElement (T.pack "<circle r=\"5\" transform=\"translate(10,20)\"/>") of
        Right (ETranslate tx ty (ECircle _)) -> do
          _ <- assertApprox "tx" epsilon 10 tx
          assertApprox "ty" epsilon 20 ty
        other -> Left ("unexpected: " ++ show other)
    ),
    ( "parseSvg document",
      case parseSvg (T.pack "<svg width=\"200\" height=\"100\"><circle r=\"50\"/></svg>") of
        Right doc -> do
          _ <- assertApprox "doc width" epsilon 200 (docWidth doc)
          assertApprox "doc height" epsilon 100 (docHeight doc)
        Left err -> Left ("parse error: " ++ show err)
    ),
    ( "parseSvg with viewBox",
      case parseSvg (T.pack "<svg width=\"200\" height=\"100\" viewBox=\"0 0 400 200\"><circle r=\"5\"/></svg>") of
        Right doc ->
          case docViewBox doc of
            Just (ViewBox mx my vw vh) -> do
              _ <- assertApprox "vb minX" epsilon 0 mx
              _ <- assertApprox "vb minY" epsilon 0 my
              _ <- assertApprox "vb w" epsilon 400 vw
              assertApprox "vb h" epsilon 200 vh
            Nothing -> Left "expected viewBox"
        Left err -> Left ("parse error: " ++ show err)
    ),
    ( "round-trip circle preserves structure",
      let original = fill red (circle 50)
          svg = renderElement original
       in case parseElement svg of
            Right parsed ->
              case parsed of
                EFill (SolidFill _) (ECircle r) ->
                  assertApprox "round-trip r" epsilon 50 r
                other -> Left ("unexpected round-trip: " ++ show other)
            Left err -> Left ("parse error: " ++ show err)
    ),
    ( "round-trip rect preserves dimensions",
      let original = rect 100 50
          svg = renderElement original
       in case parseElement svg of
            Right (ERect w h) -> do
              _ <- assertApprox "round-trip w" epsilon 100 w
              assertApprox "round-trip h" epsilon 50 h
            other -> Left ("unexpected: " ++ show other)
    )
  ]

-- ---------------------------------------------------------------------------
-- Accessibility tests
-- ---------------------------------------------------------------------------

testAccessibility :: [(String, TestResult)]
testAccessibility =
  [ ( "title wraps element",
      case title (T.pack "My Circle") (circle 50) of
        ETitle t (ECircle r) -> do
          _ <- assertEqual "title text" (T.pack "My Circle") t
          assertApprox "title r" epsilon 50 r
        other -> Left ("unexpected: " ++ show other)
    ),
    ( "desc wraps element",
      case desc (T.pack "A big circle") (circle 50) of
        EDesc d (ECircle r) -> do
          _ <- assertEqual "desc text" (T.pack "A big circle") d
          assertApprox "desc r" epsilon 50 r
        other -> Left ("unexpected: " ++ show other)
    ),
    ( "render title produces title tag",
      let svg = renderElement (title (T.pack "Test Title") (circle 5))
       in assertContains "title tag" (T.pack "<title>Test Title</title>") svg
    ),
    ( "render desc produces desc tag",
      let svg = renderElement (desc (T.pack "Test Description") (rect 10 20))
       in assertContains "desc tag" (T.pack "<desc>Test Description</desc>") svg
    ),
    ( "title escapes XML",
      let svg = renderElement (title (T.pack "A & B < C") (circle 5))
       in assertContains "escaped title" (T.pack "&amp;") svg
    )
  ]

-- ---------------------------------------------------------------------------
-- Optimization tests
-- ---------------------------------------------------------------------------

testOptimize :: [(String, TestResult)]
testOptimize =
  [ ( "optimize collapses nested translates",
      let el = ETranslate 10 20 (ETranslate 30 40 (ECircle 5))
          opt = optimizeElement el
       in assertEqual "merged translate" (ETranslate 40 60 (ECircle 5)) opt
    ),
    ( "optimize removes identity translate",
      let el = ETranslate 0 0 (ECircle 5)
          opt = optimizeElement el
       in assertEqual "no translate" (ECircle 5) opt
    ),
    ( "optimize collapses nested scales",
      let el = EScale 2 3 (EScale 4 5 (ECircle 5))
          opt = optimizeElement el
       in assertEqual "merged scale" (EScale 8 15 (ECircle 5)) opt
    ),
    ( "optimize removes identity scale",
      let el = EScale 1 1 (ECircle 5)
          opt = optimizeElement el
       in assertEqual "no scale" (ECircle 5) opt
    ),
    ( "optimize removes identity opacity",
      let el = EOpacity 1 (ECircle 5)
          opt = optimizeElement el
       in assertEqual "no opacity" (ECircle 5) opt
    ),
    ( "optimize flattens singleton group",
      let el = EGroup [ECircle 5]
          opt = optimizeElement el
       in assertEqual "singleton" (ECircle 5) opt
    ),
    ( "optimize removes EEmpty from groups",
      let el = EGroup [EEmpty, ECircle 5, EEmpty]
          opt = optimizeElement el
       in assertEqual "no empties" (ECircle 5) opt
    ),
    ( "optimize empty group becomes EEmpty",
      let el = EGroup [EEmpty, EEmpty]
          opt = optimizeElement el
       in assertEqual "all empty" EEmpty opt
    ),
    ( "optimize preserves non-identity translate",
      let el = ETranslate 10 20 (ECircle 5)
          opt = optimizeElement el
       in assertEqual "kept translate" (ETranslate 10 20 (ECircle 5)) opt
    ),
    ( "optimize deep nesting",
      let el = ETranslate 0 0 (EScale 1 1 (EOpacity 1 (ECircle 5)))
          opt = optimizeElement el
       in assertEqual "deep nesting" (ECircle 5) opt
    )
  ]

-- ---------------------------------------------------------------------------
-- SVG file output test
-- ---------------------------------------------------------------------------

testSvgFile :: IO [(String, TestResult)]
testSvgFile = do
  let doc = document 200 200 (fill red (circle 50))
  (path, h) <- openTempFile "." "gb-vector-test.svg"
  hClose h
  writeSvg path doc
  content <- TIO.readFile path
  return
    [ ( "writeSvg produces valid SVG",
        assertContains "svg tag" (T.pack "<svg") content
      ),
      ( "writeSvg includes circle",
        assertContains "circle in file" (T.pack "<circle") content
      ),
      ( "writeSvg includes fill",
        assertContains "fill in file" (T.pack "fill=\"#ff0000\"") content
      )
    ]

-- ---------------------------------------------------------------------------
-- Constants
-- ---------------------------------------------------------------------------

epsilon :: Double
epsilon = 1.0e-10

-- | Tolerance for color space conversions (gamma curves introduce small errors).
colorTolerance :: Double
colorTolerance = 1.0e-6
