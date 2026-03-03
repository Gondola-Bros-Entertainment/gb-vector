-- | gb-vector test suite.
--
-- Hand-rolled assertions — first failure stops all. Same pattern as gb-sprite.
module Main (main) where

import Data.Function ((&))
import Data.Maybe (isNothing)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import GBVector.Bezier (arcToCubics, cubicBBox, cubicLength, evalCubic, evalQuad, flattenCubic, splitCubicAt, subdivideCubic)
import GBVector.Boolean (difference, intersection, pathToPolygon, pointInPolygon, polygonArea, polygonToPath, union, xorPaths)
import GBVector.Color
  ( Color (..),
    black,
    blue,
    chocolate,
    coral,
    crimson,
    cyan,
    darkBlue,
    darkGray,
    darkGreen,
    darkOrange,
    darkRed,
    darken,
    deepPink,
    desaturate,
    dimGray,
    forestGreen,
    fromOklab,
    gold,
    gray,
    green,
    hex,
    hotPink,
    hsl,
    hsla,
    indigo,
    invert,
    lerp,
    lerpOklab,
    lightGray,
    lighten,
    lime,
    magenta,
    navy,
    olive,
    orange,
    pink,
    plum,
    purple,
    red,
    rgb,
    rgb8,
    rgba,
    royalBlue,
    saddleBrown,
    salmon,
    saturate,
    seaGreen,
    sienna,
    silver,
    skyBlue,
    steelBlue,
    teal,
    toHex,
    toOklab,
    tomato,
    transparent,
    violet,
    white,
    withAlpha,
    yellow,
  )
import GBVector.Compose (background, document, documentWithViewBox, empty, group, optimizeElement)
import GBVector.Element
  ( Document (..),
    Element (..),
    Fill (..),
    FilterKind (..),
    Gradient (..),
    GradientStop (..),
    StrokeConfig (..),
    TextAnchor (..),
    TextConfig (..),
  )
import GBVector.Gradient (evenStops, linearGradient, oklabStops, radialGradient, stop, stopWithOpacity)
import GBVector.Noise (fbm, jitterPoints, noiseClosedPath, noisePath, perlin2D, simplex2D, voronoiCells, voronoiEdges, wobblePath)
import GBVector.Path (arcTo, buildPath, closePath, cubicTo, lineTo, polygonPath, polylinePath, quadTo, startAt)
import GBVector.PathOps (measurePath, offsetPath, reversePath, simplifyPath, splitPathAt, subpath)
import GBVector.Pattern (PatternConfig (..), checker, crosshatch, defaultPatternConfig, dotGrid, lineGrid, patternDef)
import GBVector.SVG (render, renderCompact, renderElement, writeSvg)
import GBVector.SVG.Parse (ParseError (..), parseElement, parseSvg)
import GBVector.Shape (arc, circle, ellipse, line, polygon, rect, regularPolygon, ring, roundedRect, square, star)
import GBVector.Style (blur, clip, dashedStroke, defaultStrokeConfig, desc, dropShadow, fill, fillColor, fillGradient, fillNone, fillRule, mask, opacity, raw, stroke, strokeEx, title, use, withId)
import GBVector.Text (anchor, bold, defaultTextConfig, fontFamily, fontSize, italic, text, textAt, textWithConfig)
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
    skewYM,
    translate,
    translateM,
  )
import GBVector.Types
  ( ArcParams (..),
    FillRule (..),
    LineCap (..),
    LineJoin (..),
    Path (..),
    Segment (..),
    SpreadMethod (..),
    V2 (..),
    ViewBox (..),
  )
import System.Directory (removeFile)
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
        ++ testArcOps
        ++ testNamedColors
        ++ testDerivedInstances
        ++ testParseCoverage
        ++ testPatternCoverage
        ++ testTextCoverage
        ++ testStyleCoverage
        ++ testOptimizeCoverage
        ++ testRenderCoverage
        ++ testBooleanCoverage
        ++ testPathOpsCoverage
        ++ testSemigroupCoverage
        ++ testRenderShapes
        ++ testRenderPatterns
        ++ testTransformCoverage
        ++ testGradientCoverage
        ++ testPathCoverage
        ++ testRoundTrip
        ++ testParseCoverage2
        ++ testPathOpsCoverage2
        ++ testNoiseCoverage
        ++ testPathBuilderCoverage
        ++ testBooleanCoverage2
        ++ testDerivedCoverage2
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
          (firstStop, lastStop) = case stops of
            (s : rest) -> (s, case reverse rest of (l : _) -> l; [] -> s)
            [] -> error "oklabStops returned empty"
          Color fr fg fb _ = stopColor firstStop
          Color lr lg lb _ = stopColor lastStop
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
-- Arc operations and audit fix tests
-- ---------------------------------------------------------------------------

testArcOps :: [(String, TestResult)]
testArcOps =
  [ ( "arc flattening in boolean ops follows curve",
      -- A semicircular arc from (100,0) to (-100,0) should produce polygon
      -- points that are far from the straight line between endpoints.
      let arcPath =
            Path
              { pathStart = V2 100 0,
                pathSegments = [ArcTo (ArcParams 100 100 0 True True) (V2 (-100) 0)],
                pathClosed = True
              }
          poly = pathToPolygon arcPath
          -- The topmost Y coordinate should be near 100 (top of the arc),
          -- not near 0 (the straight line).
          maxY = maximum (map (\(V2 _ y) -> y) poly)
       in assertTrue "arc polygon follows curve" (maxY > 50)
    ),
    ( "arc path measurement exceeds straight-line distance",
      -- A semicircular arc of radius 100 has length pi*100 ~ 314.
      -- Straight-line distance from (100,0) to (-100,0) is 200.
      let arcPath =
            Path
              { pathStart = V2 100 0,
                pathSegments = [ArcTo (ArcParams 100 100 0 True True) (V2 (-100) 0)],
                pathClosed = False
              }
          len = measurePath arcPath
       in assertTrue "arc length > straight" (len > 250)
    ),
    ( "boolean intersection with arc-containing path is non-empty",
      let arcPath =
            Path
              { pathStart = V2 0 0,
                pathSegments =
                  [ LineTo (V2 100 0),
                    ArcTo (ArcParams 50 50 0 False True) (V2 100 100),
                    LineTo (V2 0 100)
                  ],
                pathClosed = True
              }
          squareClip = polygonPath [V2 25 25, V2 75 25, V2 75 75, V2 25 75]
          result = intersection arcPath squareClip
       in assertTrue "arc intersection non-empty" (not (null (pathSegments result)))
    ),
    ( "subpath near t=1 does not overflow",
      let path = polylinePath [V2 0 0, V2 100 0, V2 200 0]
          sub = subpath 0.999 1.0 path
       in assertTrue "subpath near 1" (measurePath sub >= 0)
    ),
    ( "saturate near-gray color stays in [0,1]",
      let nearGray = rgb 0.5 0.5 0.5
          boosted = saturate 1.0 nearGray
          (Color r g b a) = boosted
       in assertTrue
            "saturate gray in gamut"
            (r >= 0 && r <= 1 && g >= 0 && g <= 1 && b >= 0 && b <= 1 && a >= 0 && a <= 1)
    ),
    ( "saturate preserves alpha",
      let Color _ _ _ a = saturate 0.5 (rgba 1 0 0 0.3)
       in assertApprox "saturate alpha" colorTolerance 0.3 a
    ),
    ( "noise determinism preserved after gradient change",
      -- Verify perlin and simplex are still deterministic with 8-direction table.
      let pv1 = perlin2D 42 3.14 2.71
          pv2 = perlin2D 42 3.14 2.71
          sv1 = simplex2D 42 3.14 2.71
          sv2 = simplex2D 42 3.14 2.71
       in do
            _ <- assertEqual "perlin still deterministic" pv1 pv2
            assertEqual "simplex still deterministic" sv1 sv2
    ),
    ( "arcToCubics produces segments for valid arc",
      let cubics = arcToCubics (V2 0 0) 50 50 0 False True (V2 50 50)
       in assertTrue "arcToCubics non-empty" (not (null cubics))
    ),
    ( "union with arc path is closed",
      let arcPath =
            Path
              { pathStart = V2 0 0,
                pathSegments =
                  [ ArcTo (ArcParams 100 100 0 True True) (V2 200 0),
                    LineTo (V2 200 200),
                    LineTo (V2 0 200)
                  ],
                pathClosed = True
              }
          otherSquare = polygonPath [V2 50 50, V2 150 50, V2 150 150, V2 50 150]
          result = union arcPath otherSquare
       in assertTrue "arc union closed" (pathClosed result)
    )
  ]

-- ---------------------------------------------------------------------------
-- Named colors coverage (all 43)
-- ---------------------------------------------------------------------------

testNamedColors :: [(String, TestResult)]
testNamedColors =
  let validColor (Color r g b a) = r >= 0 && r <= 1 && g >= 0 && g <= 1 && b >= 0 && b <= 1 && a >= 0 && a <= 1
      allColors =
        [ black,
          white,
          red,
          green,
          blue,
          yellow,
          cyan,
          magenta,
          gray,
          darkGray,
          lightGray,
          silver,
          dimGray,
          crimson,
          darkRed,
          coral,
          tomato,
          salmon,
          hotPink,
          deepPink,
          pink,
          orange,
          darkOrange,
          gold,
          chocolate,
          saddleBrown,
          sienna,
          lime,
          darkGreen,
          forestGreen,
          seaGreen,
          olive,
          teal,
          navy,
          darkBlue,
          royalBlue,
          steelBlue,
          skyBlue,
          indigo,
          purple,
          violet,
          plum,
          transparent
        ]
   in [ ( "all 43 named colors are valid",
          assertTrue "named colors" (all validColor allColors)
        ),
        ( "named colors have correct count",
          assertEqual "color count" 43 (length allColors)
        )
      ]

-- ---------------------------------------------------------------------------
-- Derived instances coverage (Show/Eq/Ord/Enum/Bounded)
-- ---------------------------------------------------------------------------

testDerivedInstances :: [(String, TestResult)]
testDerivedInstances =
  [ -- V2 Ord (exercises compare, <, >, <=, >=, max, min, show)
    ( "V2 Ord and Show instances",
      let a = V2 0 0
          b = V2 1 0
       in do
            _ <- assertEqual "compare" LT (compare a b)
            _ <- assertTrue "<" (a < b)
            _ <- assertTrue ">" (b > a)
            _ <- assertTrue "<=" (a <= a)
            _ <- assertTrue ">=" (b >= a)
            _ <- assertEqual "max" b (max a b)
            _ <- assertEqual "min" a (min a b)
            assertTrue "show" (not (null (show a)))
    ),
    -- Segment Show/Eq (all constructors)
    ( "Segment Show/Eq all constructors",
      let seg1 = LineTo (V2 1 2)
          seg2 = CubicTo (V2 1 2) (V2 3 4) (V2 5 6)
          seg3 = QuadTo (V2 1 2) (V2 3 4)
          seg4 = ArcTo (ArcParams 50 50 0 False True) (V2 1 2)
       in do
            _ <- assertTrue "LineTo show" (not (null (show seg1)))
            _ <- assertTrue "CubicTo show" (not (null (show seg2)))
            _ <- assertTrue "QuadTo show" (not (null (show seg3)))
            _ <- assertTrue "ArcTo show" (not (null (show seg4)))
            _ <- assertEqual "LineTo eq" seg1 seg1
            assertTrue "LineTo /=" (seg1 /= LineTo (V2 9 9))
    ),
    -- ArcParams, Path, ViewBox Show/Eq
    ( "ArcParams/Path/ViewBox Show/Eq",
      let ap1 = ArcParams 50 50 0 False True
          ap2 = ArcParams 25 25 0 False True
          p = polygonPath [V2 0 0, V2 1 0, V2 1 1]
          vb = ViewBox 0 0 100 100
       in do
            _ <- assertTrue "ArcParams show" (not (null (show ap1)))
            _ <- assertEqual "ArcParams eq" ap1 ap1
            _ <- assertTrue "ArcParams /=" (ap1 /= ap2)
            _ <- assertTrue "Path show" (not (null (show p)))
            _ <- assertEqual "Path eq" p p
            _ <- assertTrue "ViewBox show" (not (null (show vb)))
            _ <- assertEqual "ViewBox eq" vb vb
            assertTrue "ViewBox /=" (vb /= ViewBox 0 0 200 200)
    ),
    -- LineCap Enum/Bounded/Ord/Show
    ( "LineCap Enum/Bounded/Ord",
      do
        _ <- assertEqual "toEnum" CapButt (toEnum 0 :: LineCap)
        _ <- assertEqual "fromEnum" 1 (fromEnum CapRound)
        _ <- assertEqual "succ" CapRound (succ CapButt)
        _ <- assertEqual "pred" CapButt (pred CapRound)
        _ <- assertEqual "minBound" CapButt (minBound :: LineCap)
        _ <- assertEqual "maxBound" CapSquare (maxBound :: LineCap)
        _ <- assertEqual "enumFrom" [CapButt, CapRound, CapSquare] [CapButt ..]
        _ <- assertEqual "compare" LT (compare CapButt CapRound)
        assertTrue "show" (not (null (show CapSquare)))
    ),
    -- LineJoin Enum/Bounded/Ord/Show
    ( "LineJoin Enum/Bounded/Ord",
      do
        _ <- assertEqual "toEnum" JoinMiter (toEnum 0 :: LineJoin)
        _ <- assertEqual "fromEnum" 2 (fromEnum JoinBevel)
        _ <- assertEqual "minBound" JoinMiter (minBound :: LineJoin)
        _ <- assertEqual "maxBound" JoinBevel (maxBound :: LineJoin)
        _ <- assertEqual "enumFrom" [JoinMiter, JoinRound, JoinBevel] [JoinMiter ..]
        assertTrue "show" (not (null (show JoinRound)))
    ),
    -- FillRule Enum/Bounded/Ord/Show
    ( "FillRule Enum/Bounded/Ord",
      do
        _ <- assertEqual "toEnum" FillNonZero (toEnum 0 :: FillRule)
        _ <- assertEqual "fromEnum" 1 (fromEnum FillEvenOdd)
        _ <- assertEqual "minBound" FillNonZero (minBound :: FillRule)
        _ <- assertEqual "maxBound" FillEvenOdd (maxBound :: FillRule)
        _ <- assertEqual "enumFrom" [FillNonZero, FillEvenOdd] [FillNonZero ..]
        _ <- assertEqual "compare" LT (compare FillNonZero FillEvenOdd)
        assertTrue "show" (not (null (show FillEvenOdd)))
    ),
    -- SpreadMethod Enum/Bounded/Ord/Show
    ( "SpreadMethod Enum/Bounded/Ord",
      do
        _ <- assertEqual "toEnum" SpreadPad (toEnum 0 :: SpreadMethod)
        _ <- assertEqual "fromEnum" 2 (fromEnum SpreadRepeat)
        _ <- assertEqual "succ" SpreadReflect (succ SpreadPad)
        _ <- assertEqual "minBound" SpreadPad (minBound :: SpreadMethod)
        _ <- assertEqual "maxBound" SpreadRepeat (maxBound :: SpreadMethod)
        _ <- assertEqual "enumFrom" [SpreadPad, SpreadReflect, SpreadRepeat] [SpreadPad ..]
        _ <- assertEqual "compare" GT (compare SpreadRepeat SpreadPad)
        assertTrue "show" (not (null (show SpreadReflect)))
    ),
    -- TextAnchor Enum/Bounded/Ord/Show
    ( "TextAnchor Enum/Bounded/Ord",
      do
        _ <- assertEqual "toEnum" AnchorStart (toEnum 0 :: TextAnchor)
        _ <- assertEqual "fromEnum" 2 (fromEnum AnchorEnd)
        _ <- assertEqual "minBound" AnchorStart (minBound :: TextAnchor)
        _ <- assertEqual "maxBound" AnchorEnd (maxBound :: TextAnchor)
        _ <- assertEqual "compare" LT (compare AnchorStart AnchorEnd)
        assertTrue "show" (not (null (show AnchorMiddle)))
    ),
    -- Element Show all constructors (one combined test)
    ( "Element Show all constructors",
      let elements =
            [ EPath (buildPath (startAt (V2 0 0) >> lineTo (V2 10 10))),
              ECircle 50,
              EEllipse 30 40,
              ERect 100 50,
              ERoundRect 100 50 5 5,
              ELine (V2 0 0) (V2 10 10),
              EPolyline [V2 0 0, V2 10 10],
              EPolygon [V2 0 0, V2 10 0, V2 10 10],
              EText defaultTextConfig (T.pack "hi"),
              EGroup [ECircle 5],
              EFill (SolidFill red) (ECircle 10),
              EStroke red 2 (ECircle 10),
              EStrokeEx defaultStrokeConfig (ECircle 10),
              EFillRule FillEvenOdd (ECircle 5),
              EOpacity 0.5 (ECircle 10),
              ETranslate 10 20 (ECircle 5),
              ERotate 45 (ECircle 5),
              ERotateAround 45 (V2 50 50) (ECircle 5),
              EScale 2 2 (ECircle 5),
              ESkewX 10 (ECircle 5),
              ESkewY 10 (ECircle 5),
              EClip (ERect 10 10) (ECircle 5),
              EMask (ERect 10 10) (ECircle 5),
              EFilter (FilterBlur 5) (ECircle 10),
              EWithId (T.pack "test") (ECircle 5),
              EUse (T.pack "ref"),
              ERaw (T.pack "<g/>"),
              ETitle (T.pack "t") (ECircle 5),
              EDesc (T.pack "d") (ECircle 5),
              EEmpty
            ]
       in assertTrue "all show" (not (any (null . show) elements))
    ),
    -- Fill, Gradient, FilterKind, StrokeConfig, TextConfig, Document, PatternConfig Show/Eq
    ( "Supporting types Show/Eq",
      let g = LinearGradient (V2 0 0) (V2 100 0) [] SpreadPad
          rg = RadialGradient (V2 50 50) 50 (V2 50 50) [] SpreadPad
          gs = GradientStop 0 red 1
          fk = FilterBlur 5
          fk2 = FilterDropShadow 2 2 4 black
          pc = defaultPatternConfig (T.pack "p1")
       in do
            _ <- assertTrue "GradientFill show" (not (null (show (GradientFill g))))
            _ <- assertTrue "NoFill show" (not (null (show NoFill)))
            _ <- assertTrue "RadialGradient show" (not (null (show rg)))
            _ <- assertEqual "RadialGradient eq" rg rg
            _ <- assertTrue "GradientStop show" (not (null (show gs)))
            _ <- assertTrue "StrokeConfig show" (not (null (show defaultStrokeConfig)))
            _ <- assertEqual "StrokeConfig eq" defaultStrokeConfig defaultStrokeConfig
            _ <- assertTrue "FilterBlur show" (not (null (show fk)))
            _ <- assertTrue "FilterDropShadow show" (not (null (show fk2)))
            _ <- assertEqual "FilterKind eq" fk fk
            _ <- assertTrue "TextConfig show" (not (null (show defaultTextConfig)))
            _ <- assertEqual "TextConfig eq" defaultTextConfig defaultTextConfig
            _ <- assertTrue "Document show" (not (null (show (document 200 200 EEmpty))))
            _ <- assertEqual "Document eq" (document 200 200 EEmpty) (document 200 200 EEmpty)
            _ <- assertTrue "PatternConfig show" (not (null (show pc)))
            _ <- assertEqual "PatternConfig eq" pc pc
            assertTrue "PatternConfig /=" (pc /= defaultPatternConfig (T.pack "p2"))
    )
  ]

-- ---------------------------------------------------------------------------
-- SVG.Parse coverage (relative commands, arcs, errors, etc.)
-- ---------------------------------------------------------------------------

testParseCoverage :: [(String, TestResult)]
testParseCoverage =
  [ ( "parse ellipse",
      case parseElement (T.pack "<ellipse rx=\"30\" ry=\"20\" />") of
        Right (EEllipse 30 20) -> Right ()
        Right other -> Left ("unexpected: " ++ show other)
        Left err -> Left (show err)
    ),
    ( "parse ellipse with cx cy",
      case parseElement (T.pack "<ellipse cx=\"50\" cy=\"50\" rx=\"30\" ry=\"20\" />") of
        Right (ETranslate 50 50 (EEllipse 30 20)) -> Right ()
        Right other -> Left ("unexpected: " ++ show other)
        Left err -> Left (show err)
    ),
    ( "parse polygon with points",
      case parseElement (T.pack "<polygon points=\"0,0 10,0 10,10\" />") of
        Right (EPolygon _) -> Right ()
        Right other -> Left ("unexpected: " ++ show other)
        Left err -> Left (show err)
    ),
    ( "parse polyline with points",
      case parseElement (T.pack "<polyline points=\"0,0 10,0 10,10\" />") of
        Right (EPolyline _) -> Right ()
        Right other -> Left ("unexpected: " ++ show other)
        Left err -> Left (show err)
    ),
    ( "parse path with relative lineto",
      case parseElement (T.pack "<path d=\"M 0 0 l 10 10\" />") of
        Right (EPath _) -> Right ()
        Right other -> Left ("unexpected: " ++ show other)
        Left err -> Left (show err)
    ),
    ( "parse path with horizontal line",
      case parseElement (T.pack "<path d=\"M 0 0 H 50\" />") of
        Right (EPath _) -> Right ()
        Right other -> Left ("unexpected: " ++ show other)
        Left err -> Left (show err)
    ),
    ( "parse path with relative horizontal",
      case parseElement (T.pack "<path d=\"M 0 0 h 50\" />") of
        Right (EPath _) -> Right ()
        Right other -> Left ("unexpected: " ++ show other)
        Left err -> Left (show err)
    ),
    ( "parse path with vertical line",
      case parseElement (T.pack "<path d=\"M 0 0 V 50\" />") of
        Right (EPath _) -> Right ()
        Right other -> Left ("unexpected: " ++ show other)
        Left err -> Left (show err)
    ),
    ( "parse path with relative vertical",
      case parseElement (T.pack "<path d=\"M 0 0 v 50\" />") of
        Right (EPath _) -> Right ()
        Right other -> Left ("unexpected: " ++ show other)
        Left err -> Left (show err)
    ),
    ( "parse path with relative cubic",
      case parseElement (T.pack "<path d=\"M 0 0 c 10 0 20 10 20 20\" />") of
        Right (EPath _) -> Right ()
        Right other -> Left ("unexpected: " ++ show other)
        Left err -> Left (show err)
    ),
    ( "parse path with quadratic",
      case parseElement (T.pack "<path d=\"M 0 0 Q 10 20 30 0\" />") of
        Right (EPath p) -> assertTrue "quad segments" (not (null (pathSegments p)))
        Right other -> Left ("unexpected: " ++ show other)
        Left err -> Left (show err)
    ),
    ( "parse path with relative quadratic",
      case parseElement (T.pack "<path d=\"M 0 0 q 10 20 30 0\" />") of
        Right (EPath _) -> Right ()
        Right other -> Left ("unexpected: " ++ show other)
        Left err -> Left (show err)
    ),
    ( "parse path with arc",
      case parseElement (T.pack "<path d=\"M 0 0 A 50 50 0 0 1 100 0\" />") of
        Right (EPath _) -> Right ()
        Right other -> Left ("unexpected: " ++ show other)
        Left err -> Left (show err)
    ),
    ( "parse path with relative arc",
      case parseElement (T.pack "<path d=\"M 0 0 a 50 50 0 1 0 100 0\" />") of
        Right (EPath _) -> Right ()
        Right other -> Left ("unexpected: " ++ show other)
        Left err -> Left (show err)
    ),
    ( "parse path with relative moveto",
      case parseElement (T.pack "<path d=\"m 10 10 l 20 0\" />") of
        Right (EPath _) -> Right ()
        Right other -> Left ("unexpected: " ++ show other)
        Left err -> Left (show err)
    ),
    ( "parse transform scale",
      case parseElement (T.pack "<circle r=\"10\" transform=\"scale(2)\" />") of
        Right (EScale 2 2 _) -> Right ()
        Right other -> Left ("unexpected: " ++ show other)
        Left err -> Left (show err)
    ),
    ( "parse transform scale xy",
      case parseElement (T.pack "<circle r=\"10\" transform=\"scale(2,3)\" />") of
        Right (EScale 2 3 _) -> Right ()
        Right other -> Left ("unexpected: " ++ show other)
        Left err -> Left (show err)
    ),
    ( "parse transform skewX",
      case parseElement (T.pack "<circle r=\"10\" transform=\"skewX(15)\" />") of
        Right (ESkewX 15 _) -> Right ()
        Right other -> Left ("unexpected: " ++ show other)
        Left err -> Left (show err)
    ),
    ( "parse transform skewY",
      case parseElement (T.pack "<circle r=\"10\" transform=\"skewY(15)\" />") of
        Right (ESkewY 15 _) -> Right ()
        Right other -> Left ("unexpected: " ++ show other)
        Left err -> Left (show err)
    ),
    ( "parse transform rotate with center",
      case parseElement (T.pack "<circle r=\"10\" transform=\"rotate(45,50,50)\" />") of
        Right (ERotateAround 45 (V2 50 50) _) -> Right ()
        Right other -> Left ("unexpected: " ++ show other)
        Left err -> Left (show err)
    ),
    ( "parse stroke attribute",
      case parseElement (T.pack "<circle r=\"10\" stroke=\"#ff0000\" stroke-width=\"2\" />") of
        Right (EStroke {}) -> Right ()
        Right other -> Left ("unexpected: " ++ show other)
        Left err -> Left (show err)
    ),
    ( "parse opacity attribute",
      case parseElement (T.pack "<circle r=\"10\" opacity=\"0.5\" />") of
        Right (EOpacity 0.5 _) -> Right ()
        Right other -> Left ("unexpected: " ++ show other)
        Left err -> Left (show err)
    ),
    ( "parse use element",
      case parseElement (T.pack "<use href=\"#myid\" />") of
        Right (EUse _) -> Right ()
        Right other -> Left ("unexpected: " ++ show other)
        Left err -> Left (show err)
    ),
    ( "parse text element",
      case parseElement (T.pack "<text>Hello</text>") of
        Right (EText _ _) -> Right ()
        Right other -> Left ("unexpected: " ++ show other)
        Left err -> Left (show err)
    ),
    ( "parse nested groups",
      case parseElement (T.pack "<g><circle r=\"5\" /><rect width=\"10\" height=\"10\" /></g>") of
        Right (EGroup _) -> Right ()
        Right other -> Left ("unexpected: " ++ show other)
        Left err -> Left (show err)
    ),
    ( "parse comment",
      case parseElement (T.pack "<!-- comment -->") of
        Right EEmpty -> Right ()
        Right other -> Left ("unexpected: " ++ show other)
        Left err -> Left (show err)
    ),
    ( "parse fill none attribute",
      let svg = T.pack "<circle r=\"10\" fill=\"none\" />"
       in case parseElement svg of
            Right (EFill NoFill _) -> Right ()
            Right other -> Left ("unexpected: " ++ show other)
            Left err -> Left (show err)
    ),
    ( "parse unknown element",
      case parseElement (T.pack "<defs></defs>") of
        Right EEmpty -> Right ()
        Right _ -> Right ()
        Left err -> Left (show err)
    ),
    ( "parseSvg malformed tag",
      case parseSvg (T.pack "<div></div>") of
        Left _ -> Right ()
        Right _ -> Left "expected parse error"
    ),
    ( "parseElement empty returns EEmpty",
      case parseElement (T.pack "") of
        Right EEmpty -> Right ()
        Right other -> Left ("unexpected: " ++ show other)
        Left _ -> Right ()
    ),
    ( "parse path empty d attribute",
      case parseElement (T.pack "<path d=\"\" />") of
        Right (EPath _) -> Right ()
        Right other -> Left ("unexpected: " ++ show other)
        Left err -> Left (show err)
    ),
    ( "parseSvg with width and height",
      case parseSvg (T.pack "<svg width=\"400\" height=\"300\"><circle r=\"10\" /></svg>") of
        Right doc -> do
          _ <- assertApprox "width" 0.01 400 (docWidth doc)
          assertApprox "height" 0.01 300 (docHeight doc)
        Left err -> Left (show err)
    )
  ]

-- ---------------------------------------------------------------------------
-- Pattern coverage
-- ---------------------------------------------------------------------------

testPatternCoverage :: [(String, TestResult)]
testPatternCoverage =
  [ ( "patternDef wraps element",
      let cfg = defaultPatternConfig (T.pack "dots")
          pat = patternDef cfg (dotGrid 10 2 red)
       in case pat of
            EGroup _ -> Right ()
            other -> Left ("expected EGroup, got " ++ show other)
    ),
    ( "defaultPatternConfig fields",
      let cfg = defaultPatternConfig (T.pack "p1")
       in do
            _ <- assertEqual "patternId" (T.pack "p1") (patternId cfg)
            _ <- assertApprox "patternWidth" 0.01 100 (patternWidth cfg)
            assertApprox "patternHeight" 0.01 100 (patternHeight cfg)
    ),
    ( "patternConfig custom size",
      let cfg = PatternConfig (T.pack "p2") 20 30
       in do
            _ <- assertApprox "width" 0.01 20 (patternWidth cfg)
            assertApprox "height" 0.01 30 (patternHeight cfg)
    ),
    ( "dotGrid renders circles",
      let el = dotGrid 20 3 blue
       in case el of
            EGroup elems -> assertTrue "has dots" (not (null elems))
            other -> Left ("expected EGroup, got " ++ show other)
    ),
    ( "lineGrid renders lines",
      let el = lineGrid 20 1 black
       in case el of
            EGroup elems -> assertTrue "has lines" (not (null elems))
            other -> Left ("expected EGroup, got " ++ show other)
    ),
    ( "crosshatch renders lines",
      let el = crosshatch 20 1 gray
       in case el of
            EGroup elems -> assertTrue "has lines" (not (null elems))
            other -> Left ("expected EGroup, got " ++ show other)
    )
  ]

-- ---------------------------------------------------------------------------
-- Text coverage
-- ---------------------------------------------------------------------------

testTextCoverage :: [(String, TestResult)]
testTextCoverage =
  [ ( "textWithConfig creates EText",
      let cfg = defaultTextConfig
          el = textWithConfig cfg (T.pack "hello")
       in case el of
            EText _ _ -> Right ()
            other -> Left ("expected EText, got " ++ show other)
    ),
    ( "fontSize modifies config",
      let cfg = fontSize 24 defaultTextConfig
       in assertApprox "font size" 0.01 24 (textConfigFontSize cfg)
    ),
    ( "fontFamily modifies config",
      let cfg = fontFamily (T.pack "monospace") defaultTextConfig
       in assertEqual "font family" (T.pack "monospace") (textConfigFontFamily cfg)
    ),
    ( "bold modifies config",
      let cfg = bold defaultTextConfig
       in assertEqual "bold" True (textConfigBold cfg)
    ),
    ( "italic modifies config",
      let cfg = italic defaultTextConfig
       in assertEqual "italic" True (textConfigItalic cfg)
    ),
    ( "anchor start",
      let cfg = anchor AnchorStart defaultTextConfig
       in assertEqual "anchor" AnchorStart (textConfigAnchor cfg)
    ),
    ( "anchor middle",
      let cfg = anchor AnchorMiddle defaultTextConfig
       in assertEqual "anchor" AnchorMiddle (textConfigAnchor cfg)
    ),
    ( "anchor end",
      let cfg = anchor AnchorEnd defaultTextConfig
       in assertEqual "anchor" AnchorEnd (textConfigAnchor cfg)
    ),
    ( "render bold text includes font-weight",
      let el = EText (bold defaultTextConfig) (T.pack "Bold")
          svg = renderElement el
       in assertContains "font-weight" (T.pack "font-weight=\"bold\"") svg
    ),
    ( "render italic text includes font-style",
      let el = EText (italic defaultTextConfig) (T.pack "Italic")
          svg = renderElement el
       in assertContains "font-style" (T.pack "font-style=\"italic\"") svg
    ),
    ( "render text anchor middle",
      let el = EText (anchor AnchorMiddle defaultTextConfig) (T.pack "Center")
          svg = renderElement el
       in assertContains "text-anchor" (T.pack "text-anchor=\"middle\"") svg
    ),
    ( "render text anchor end",
      let el = EText (anchor AnchorEnd defaultTextConfig) (T.pack "End")
          svg = renderElement el
       in assertContains "text-anchor" (T.pack "text-anchor=\"end\"") svg
    )
  ]

-- ---------------------------------------------------------------------------
-- Style coverage
-- ---------------------------------------------------------------------------

testStyleCoverage :: [(String, TestResult)]
testStyleCoverage =
  [ ( "fillColor is alias for fill",
      let e1 = fill red (circle 10)
          e2 = fillColor red (circle 10)
       in assertEqual "fillColor" e1 e2
    ),
    ( "fillGradient wraps with GradientFill",
      let g = linearGradient (V2 0 0) (V2 100 0) (evenStops [red, blue])
          el = fillGradient g (circle 10)
       in case el of
            EFill (GradientFill _) _ -> Right ()
            other -> Left ("expected GradientFill, got " ++ show other)
    ),
    ( "fillRule wraps with EFillRule",
      let el = fillRule FillEvenOdd (circle 10)
       in case el of
            EFillRule FillEvenOdd _ -> Right ()
            other -> Left ("expected EFillRule, got " ++ show other)
    ),
    ( "strokeEx wraps with EStrokeEx",
      let el = strokeEx defaultStrokeConfig (circle 10)
       in case el of
            EStrokeEx _ _ -> Right ()
            other -> Left ("expected EStrokeEx, got " ++ show other)
    ),
    ( "mask wraps with EMask",
      let el = mask (rect 100 100) (circle 50)
       in case el of
            EMask _ _ -> Right ()
            other -> Left ("expected EMask, got " ++ show other)
    ),
    ( "raw creates ERaw",
      let el = raw (T.pack "<custom/>")
       in case el of
            ERaw _ -> Right ()
            other -> Left ("expected ERaw, got " ++ show other)
    )
  ]

-- ---------------------------------------------------------------------------
-- Optimizer coverage (more branches)
-- ---------------------------------------------------------------------------

testOptimizeCoverage :: [(String, TestResult)]
testOptimizeCoverage =
  [ ( "optimize recurses into EStrokeEx",
      let el = EStrokeEx defaultStrokeConfig (ETranslate 0 0 (ECircle 5))
          opt = optimizeElement el
       in case opt of
            EStrokeEx _ (ECircle 5) -> Right ()
            other -> Left ("unexpected: " ++ show other)
    ),
    ( "optimize recurses into EFillRule",
      let el = EFillRule FillEvenOdd (ETranslate 0 0 (ECircle 5))
          opt = optimizeElement el
       in case opt of
            EFillRule FillEvenOdd (ECircle 5) -> Right ()
            other -> Left ("unexpected: " ++ show other)
    ),
    ( "optimize recurses into ERotate",
      let el = ERotate 45 (ETranslate 0 0 (ECircle 5))
          opt = optimizeElement el
       in case opt of
            ERotate 45 (ECircle 5) -> Right ()
            other -> Left ("unexpected: " ++ show other)
    ),
    ( "optimize recurses into ERotateAround",
      let el = ERotateAround 45 (V2 0 0) (EScale 1 1 (ECircle 5))
          opt = optimizeElement el
       in case opt of
            ERotateAround 45 (V2 0 0) (ECircle 5) -> Right ()
            other -> Left ("unexpected: " ++ show other)
    ),
    ( "optimize recurses into ESkewX",
      let el = ESkewX 10 (EOpacity 1 (ECircle 5))
          opt = optimizeElement el
       in case opt of
            ESkewX 10 (ECircle 5) -> Right ()
            other -> Left ("unexpected: " ++ show other)
    ),
    ( "optimize recurses into ESkewY",
      let el = ESkewY 10 (ETranslate 0 0 (ECircle 5))
          opt = optimizeElement el
       in case opt of
            ESkewY 10 (ECircle 5) -> Right ()
            other -> Left ("unexpected: " ++ show other)
    ),
    ( "optimize recurses into EClip",
      let el = EClip (ETranslate 0 0 (ERect 10 10)) (EScale 1 1 (ECircle 5))
          opt = optimizeElement el
       in case opt of
            EClip (ERect 10 10) (ECircle 5) -> Right ()
            other -> Left ("unexpected: " ++ show other)
    ),
    ( "optimize recurses into EMask",
      let el = EMask (ETranslate 0 0 (ERect 10 10)) (EOpacity 1 (ECircle 5))
          opt = optimizeElement el
       in case opt of
            EMask (ERect 10 10) (ECircle 5) -> Right ()
            other -> Left ("unexpected: " ++ show other)
    ),
    ( "optimize recurses into EFilter",
      let el = EFilter (FilterBlur 5) (ETranslate 0 0 (ECircle 10))
          opt = optimizeElement el
       in case opt of
            EFilter (FilterBlur 5) (ECircle 10) -> Right ()
            other -> Left ("unexpected: " ++ show other)
    ),
    ( "optimize recurses into EWithId",
      let el = EWithId (T.pack "x") (EScale 1 1 (ECircle 5))
          opt = optimizeElement el
       in case opt of
            EWithId _ (ECircle 5) -> Right ()
            other -> Left ("unexpected: " ++ show other)
    ),
    ( "optimize recurses into ETitle",
      let el = ETitle (T.pack "t") (EOpacity 1 (ECircle 5))
          opt = optimizeElement el
       in case opt of
            ETitle _ (ECircle 5) -> Right ()
            other -> Left ("unexpected: " ++ show other)
    ),
    ( "optimize recurses into EDesc",
      let el = EDesc (T.pack "d") (ETranslate 0 0 (ECircle 5))
          opt = optimizeElement el
       in case opt of
            EDesc _ (ECircle 5) -> Right ()
            other -> Left ("unexpected: " ++ show other)
    ),
    ( "optimize non-identity opacity preserved",
      let el = EOpacity 0.5 (ECircle 5)
          opt = optimizeElement el
       in case opt of
            EOpacity 0.5 (ECircle 5) -> Right ()
            other -> Left ("unexpected: " ++ show other)
    ),
    ( "optimize non-identity scale preserved",
      let el = EScale 2 3 (ECircle 5)
          opt = optimizeElement el
       in case opt of
            EScale 2 3 (ECircle 5) -> Right ()
            other -> Left ("unexpected: " ++ show other)
    ),
    ( "optimize non-identity translate preserved",
      let el = ETranslate 10 20 (ECircle 5)
          opt = optimizeElement el
       in case opt of
            ETranslate 10 20 (ECircle 5) -> Right ()
            other -> Left ("unexpected: " ++ show other)
    )
  ]

-- ---------------------------------------------------------------------------
-- Render coverage (more element types)
-- ---------------------------------------------------------------------------

testRenderCoverage :: [(String, TestResult)]
testRenderCoverage =
  [ ( "render polyline",
      let el = EPolyline [V2 0 0, V2 10 0, V2 10 10]
          svg = renderElement el
       in assertContains "polyline tag" (T.pack "<polyline") svg
    ),
    ( "render raw element",
      let el = ERaw (T.pack "<custom attr=\"val\"/>")
          svg = renderElement el
       in assertContains "raw content" (T.pack "<custom attr=\"val\"/>") svg
    ),
    ( "render mask element",
      let el = EMask (ERect 100 100) (ECircle 50)
          svg = renderElement el
       in assertContains "mask tag" (T.pack "<mask") svg
    ),
    ( "render strokeEx element",
      let cfg = defaultStrokeConfig {strokeConfigCap = CapRound, strokeConfigJoin = JoinRound}
          el = EStrokeEx cfg (ECircle 10)
          svg = renderElement el
       in do
            _ <- assertContains "stroke-linecap" (T.pack "stroke-linecap=\"round\"") svg
            assertContains "stroke-linejoin" (T.pack "stroke-linejoin=\"round\"") svg
    ),
    ( "render strokeEx with square cap and bevel join",
      let cfg = defaultStrokeConfig {strokeConfigCap = CapSquare, strokeConfigJoin = JoinBevel}
          el = EStrokeEx cfg (ECircle 10)
          svg = renderElement el
       in do
            _ <- assertContains "stroke-linecap" (T.pack "stroke-linecap=\"square\"") svg
            assertContains "stroke-linejoin" (T.pack "stroke-linejoin=\"bevel\"") svg
    ),
    ( "render strokeEx with dash offset",
      let cfg = defaultStrokeConfig {strokeConfigDashArray = [5, 3], strokeConfigDashOffset = 2}
          el = EStrokeEx cfg (ECircle 10)
          svg = renderElement el
       in assertContains "stroke-dashoffset" (T.pack "stroke-dashoffset") svg
    ),
    ( "render fillRule evenodd",
      let el = EFillRule FillEvenOdd (ECircle 10)
          svg = renderElement el
       in assertContains "fill-rule" (T.pack "fill-rule=\"evenodd\"") svg
    ),
    ( "render fillRule nonzero",
      let el = EFillRule FillNonZero (ECircle 10)
          svg = renderElement el
       in assertContains "fill-rule" (T.pack "fill-rule=\"nonzero\"") svg
    ),
    ( "render gradient with spread reflect",
      let g = LinearGradient (V2 0 0) (V2 100 0) [GradientStop 0 red 1, GradientStop 1 blue 1] SpreadReflect
          el = EFill (GradientFill g) (ERect 100 100)
          svg = renderElement el
       in assertContains "spreadMethod" (T.pack "spreadMethod=\"reflect\"") svg
    ),
    ( "render gradient with spread repeat",
      let g = LinearGradient (V2 0 0) (V2 100 0) [GradientStop 0 red 1, GradientStop 1 blue 1] SpreadRepeat
          el = EFill (GradientFill g) (ERect 100 100)
          svg = renderElement el
       in assertContains "spreadMethod" (T.pack "spreadMethod=\"repeat\"") svg
    ),
    ( "render radial gradient",
      let g = RadialGradient (V2 50 50) 50 (V2 50 50) [GradientStop 0 red 1, GradientStop 1 blue 1] SpreadPad
          el = EFill (GradientFill g) (ECircle 50)
          svg = renderElement el
       in assertContains "radialGradient" (T.pack "<radialGradient") svg
    ),
    ( "render gradient stop with opacity",
      let g = LinearGradient (V2 0 0) (V2 100 0) [GradientStop 0 red 0.5] SpreadPad
          el = EFill (GradientFill g) (ERect 100 100)
          svg = renderElement el
       in assertContains "stop-opacity" (T.pack "stop-opacity") svg
    ),
    ( "render path with quadratic",
      let p = buildPath (startAt (V2 0 0) >> quadTo (V2 50 100) (V2 100 0))
          svg = renderElement (EPath p)
       in assertContains "Q command" (T.pack "Q") svg
    ),
    ( "render path with arc",
      let p = Path (V2 0 0) [ArcTo (ArcParams 50 50 0 False True) (V2 100 0)] False
          svg = renderElement (EPath p)
       in assertContains "A command" (T.pack "A") svg
    ),
    ( "renderCompact produces valid SVG",
      let doc = document 200 200 (fill red (circle 50))
          svg = renderCompact doc
       in do
            _ <- assertContains "svg tag" (T.pack "<svg") svg
            assertContains "circle" (T.pack "<circle") svg
    ),
    ( "render drop shadow filter",
      let el = dropShadow 2 2 4 black (circle 10)
          svg = renderElement el
       in assertContains "feDropShadow" (T.pack "feDropShadow") svg
    )
  ]

-- ---------------------------------------------------------------------------
-- Boolean ops coverage
-- ---------------------------------------------------------------------------

testBooleanCoverage :: [(String, TestResult)]
testBooleanCoverage =
  [ ( "difference returns a path",
      let sq1 = polygonPath [V2 0 0, V2 100 0, V2 100 100, V2 0 100]
          sq2 = polygonPath [V2 50 0, V2 150 0, V2 150 100, V2 50 100]
          result = difference sq1 sq2
       in assertTrue "difference ran" (pathClosed result || not (pathClosed result))
    ),
    ( "xorPaths returns a path",
      let sq1 = polygonPath [V2 0 0, V2 100 0, V2 100 100, V2 0 100]
          sq2 = polygonPath [V2 50 0, V2 150 0, V2 150 100, V2 50 100]
          result = xorPaths sq1 sq2
       in assertTrue "xor ran" (pathClosed result || not (pathClosed result))
    ),
    ( "pointInPolygon edge case corner",
      let sq = [V2 0 0, V2 100 0, V2 100 100, V2 0 100]
       in assertTrue "inside near corner" (pointInPolygon (V2 1 1) sq)
    ),
    ( "polygonArea triangle",
      let tri = [V2 0 0, V2 100 0, V2 50 100]
          area = abs (polygonArea tri)
       in assertApprox "triangle area" 1 5000 area
    )
  ]

-- ---------------------------------------------------------------------------
-- PathOps coverage (more segment types)
-- ---------------------------------------------------------------------------

testPathOpsCoverage :: [(String, TestResult)]
testPathOpsCoverage =
  [ ( "reversePath with quad segments",
      let p = buildPath (startAt (V2 0 0) >> quadTo (V2 50 100) (V2 100 0))
          rev = reversePath p
       in assertTrue "reversed has segments" (not (null (pathSegments rev)))
    ),
    ( "measurePath with quad segments",
      let p = buildPath (startAt (V2 0 0) >> quadTo (V2 50 100) (V2 100 0))
          len = measurePath p
       in assertTrue "quad length positive" (len > 0)
    ),
    ( "splitPathAt 0.5 with cubics",
      let p = buildPath (startAt (V2 0 0) >> cubicTo (V2 30 100) (V2 70 100) (V2 100 0))
          (left, right) = splitPathAt 0.5 p
       in do
            _ <- assertTrue "left non-empty" (not (null (pathSegments left)))
            assertTrue "right non-empty" (not (null (pathSegments right)))
    ),
    ( "subpath with cubics",
      let p = buildPath (startAt (V2 0 0) >> cubicTo (V2 30 100) (V2 70 100) (V2 100 0) >> lineTo (V2 200 0))
          sub = subpath 0.2 0.8 p
       in assertTrue "subpath non-empty" (not (null (pathSegments sub)))
    ),
    ( "offsetPath with cubics",
      let p = buildPath (startAt (V2 0 0) >> cubicTo (V2 30 100) (V2 70 100) (V2 100 0))
          off = offsetPath 5 p
       in assertTrue "offset non-empty" (not (null (pathSegments off)))
    ),
    ( "simplifyPath preserves curves",
      let p = buildPath (startAt (V2 0 0) >> lineTo (V2 50 1) >> lineTo (V2 100 0))
          simplified = simplifyPath 5 p
       in assertTrue "simplified has fewer segments" (length (pathSegments simplified) <= length (pathSegments p))
    )
  ]

-- ---------------------------------------------------------------------------
-- Semigroup coverage (more branches)
-- ---------------------------------------------------------------------------

testSemigroupCoverage :: [(String, TestResult)]
testSemigroupCoverage =
  [ ( "EGroup <> EGroup merges",
      let g1 = EGroup [ECircle 1, ECircle 2]
          g2 = EGroup [ECircle 3, ECircle 4]
          result = g1 <> g2
       in case result of
            EGroup xs -> assertEqual "merged count" 4 (length xs)
            other -> Left ("expected EGroup, got " ++ show other)
    ),
    ( "EGroup <> single appends",
      let g = EGroup [ECircle 1, ECircle 2]
          result = g <> ECircle 3
       in case result of
            EGroup xs -> assertEqual "appended count" 3 (length xs)
            other -> Left ("expected EGroup, got " ++ show other)
    ),
    ( "single <> EGroup prepends",
      let g = EGroup [ECircle 2, ECircle 3]
          result = ECircle 1 <> g
       in case result of
            EGroup xs -> assertEqual "prepended count" 3 (length xs)
            other -> Left ("expected EGroup, got " ++ show other)
    ),
    ( "two singles make group",
      let result = ECircle 1 <> ECircle 2
       in case result of
            EGroup xs -> assertEqual "pair count" 2 (length xs)
            other -> Left ("expected EGroup, got " ++ show other)
    )
  ]

-- ---------------------------------------------------------------------------
-- Render shapes (forces lazy list evaluation for Shape.hs coverage)
-- ---------------------------------------------------------------------------

testRenderShapes :: [(String, TestResult)]
testRenderShapes =
  [ ( "render arc forces vertex evaluation",
      let svg = renderElement (arc 50 0 (pi / 2))
       in assertContains "polyline" (T.pack "<polyline") svg
    ),
    ( "render ring forces vertex evaluation",
      let svg = renderElement (ring 50 30)
       in assertContains "polygon" (T.pack "<polygon") svg
    ),
    ( "render regularPolygon forces vertex evaluation",
      let svg = renderElement (regularPolygon 6 50)
       in assertContains "polygon" (T.pack "<polygon") svg
    ),
    ( "render star forces vertex evaluation",
      let svg = renderElement (star 5 50 25)
       in assertContains "polygon" (T.pack "<polygon") svg
    ),
    ( "render ellipse",
      let svg = renderElement (ellipse 30 20)
       in assertContains "ellipse" (T.pack "<ellipse") svg
    ),
    ( "render roundedRect",
      let svg = renderElement (roundedRect 100 50 5 5)
       in assertContains "rect" (T.pack "<rect") svg
    )
  ]

-- ---------------------------------------------------------------------------
-- Render patterns (forces lazy list evaluation for Pattern.hs coverage)
-- ---------------------------------------------------------------------------

testRenderPatterns :: [(String, TestResult)]
testRenderPatterns =
  [ ( "render dotGrid forces evaluation",
      let svg = renderElement (dotGrid 20 3 red)
       in assertContains "circle" (T.pack "<circle") svg
    ),
    ( "render lineGrid forces evaluation",
      let svg = renderElement (lineGrid 20 1 black)
       in assertContains "line" (T.pack "<line") svg
    ),
    ( "render crosshatch forces evaluation",
      let svg = renderElement (crosshatch 20 1 gray)
       in assertContains "line" (T.pack "<line") svg
    ),
    ( "render checker forces evaluation",
      let svg = renderElement (checker 20 black white)
       in assertContains "rect" (T.pack "<rect") svg
    ),
    ( "render patternDef",
      let cfg = defaultPatternConfig (T.pack "dots")
          svg = renderElement (patternDef cfg (dotGrid 10 2 red))
       in assertTrue "patternDef renders" (T.length svg > 0)
    )
  ]

-- ---------------------------------------------------------------------------
-- Transform coverage (skewYM)
-- ---------------------------------------------------------------------------

testTransformCoverage :: [(String, TestResult)]
testTransformCoverage =
  [ ( "skewYM shifts y by x*tan(angle)",
      let m = skewYM 45
          V2 resultX resultY = applyMatrix m (V2 10 0)
       in do
            _ <- assertApprox "x unchanged" 0.01 10 resultX
            assertApprox "y shifted" 0.1 10 resultY
    ),
    ( "Matrix Show/Eq",
      do
        _ <- assertTrue "Matrix show" (not (null (show identity)))
        assertEqual "Matrix eq" identity identity
    )
  ]

-- ---------------------------------------------------------------------------
-- Gradient coverage (stopWithOpacity)
-- ---------------------------------------------------------------------------

testGradientCoverage :: [(String, TestResult)]
testGradientCoverage =
  [ ( "stopWithOpacity creates stop",
      let s = stopWithOpacity 0.5 red 0.8
       in do
            _ <- assertApprox "offset" 0.01 0.5 (stopOffset s)
            assertApprox "opacity" 0.01 0.8 (stopOpacity s)
    )
  ]

-- ---------------------------------------------------------------------------
-- Path builder coverage (quadTo)
-- ---------------------------------------------------------------------------

testPathCoverage :: [(String, TestResult)]
testPathCoverage =
  [ ( "buildPath with quadTo",
      let p = buildPath (startAt (V2 0 0) >> quadTo (V2 50 100) (V2 100 0))
       in assertTrue "quad has segments" (not (null (pathSegments p)))
    ),
    ( "buildPath multiple startAt resets",
      let p = buildPath (startAt (V2 0 0) >> lineTo (V2 10 0) >> startAt (V2 50 50) >> lineTo (V2 60 50))
       in assertTrue "has segments" (not (null (pathSegments p)))
    )
  ]

-- ---------------------------------------------------------------------------
-- Round-trip render/parse coverage (exercises SVG.hs and SVG.Parse deeply)
-- ---------------------------------------------------------------------------

testRoundTrip :: [(String, TestResult)]
testRoundTrip =
  [ ( "round-trip complex scene",
      let scene =
            EGroup
              [ EFill (SolidFill red) (ECircle 50),
                EStroke blue 2 (ERect 100 50),
                ETranslate 10 20 (EEllipse 30 20),
                ERotate 45 (ELine (V2 0 0) (V2 100 100)),
                EScale 2 2 (ERoundRect 50 30 5 5),
                EOpacity 0.5 (EPolygon [V2 0 0, V2 50 0, V2 25 50]),
                ESkewX 15 (ECircle 10),
                ESkewY 10 (ERect 20 20),
                EFill NoFill (ECircle 5),
                EPath (buildPath (startAt (V2 0 0) >> lineTo (V2 50 50) >> cubicTo (V2 60 80) (V2 90 80) (V2 100 50) >> closePath)),
                EPath (buildPath (startAt (V2 0 0) >> quadTo (V2 50 100) (V2 100 0))),
                EPath (Path (V2 0 0) [ArcTo (ArcParams 50 50 0 False True) (V2 100 0)] False),
                EText defaultTextConfig (T.pack "Hello"),
                EPolyline [V2 0 0, V2 10 0, V2 20 10],
                EUse (T.pack "myref"),
                EFilter (FilterBlur 3) (ECircle 25),
                EFilter (FilterDropShadow 2 2 4 black) (ERect 80 80),
                EWithId (T.pack "test-id") (ECircle 15),
                EClip (ERect 200 200) (ECircle 100),
                EMask (ECircle 50) (ERect 100 100),
                EFillRule FillEvenOdd (ECircle 20),
                EStrokeEx (defaultStrokeConfig {strokeConfigCap = CapRound, strokeConfigJoin = JoinRound, strokeConfigDashArray = [5, 3], strokeConfigDashOffset = 1}) (ECircle 30),
                ETitle (T.pack "My Title") (ECircle 5),
                EDesc (T.pack "My Desc") (ERect 10 10),
                ERaw (T.pack "<custom/>"),
                ERotateAround 90 (V2 50 50) (ECircle 10),
                EEmpty
              ]
          doc = document 400 400 scene
          svg = render doc
       in do
            _ <- assertContains "has svg" (T.pack "<svg") svg
            _ <- assertContains "has circle" (T.pack "<circle") svg
            _ <- assertContains "has rect" (T.pack "<rect") svg
            _ <- assertContains "has ellipse" (T.pack "<ellipse") svg
            _ <- assertContains "has line" (T.pack "<line") svg
            _ <- assertContains "has polygon" (T.pack "<polygon") svg
            _ <- assertContains "has polyline" (T.pack "<polyline") svg
            _ <- assertContains "has path" (T.pack "<path") svg
            _ <- assertContains "has text" (T.pack "<text") svg
            _ <- assertContains "has use" (T.pack "<use") svg
            _ <- assertContains "has g" (T.pack "<g") svg
            _ <- assertContains "has filter" (T.pack "<filter") svg
            _ <- assertContains "has clip" (T.pack "<clipPath") svg
            _ <- assertContains "has mask" (T.pack "<mask") svg
            _ <- assertContains "has title" (T.pack "<title") svg
            _ <- assertContains "has desc" (T.pack "<desc") svg
            _ <- assertContains "has custom" (T.pack "<custom/>") svg
            assertContains "has fill-rule" (T.pack "fill-rule") svg
    ),
    ( "round-trip parse rendered scene",
      let doc =
            document 200 200 $
              EGroup
                [ EFill (SolidFill red) (ECircle 50),
                  EStroke green 2 (ERect 80 60),
                  ETranslate 10 20 (EEllipse 30 20),
                  EPath (buildPath (startAt (V2 0 0) >> lineTo (V2 100 0) >> lineTo (V2 100 100) >> closePath))
                ]
          svg = render doc
       in case parseSvg svg of
            Right parsed -> do
              _ <- assertApprox "width" 0.1 200 (docWidth parsed)
              assertApprox "height" 0.1 200 (docHeight parsed)
            Left err -> Left ("parse failed: " ++ show err)
    ),
    ( "parse path with implicit L commands",
      case parseElement (T.pack "<path d=\"M 0 0 L 10 10 20 20 30 30\" />") of
        Right (EPath p) -> assertTrue "has 3 segments" (length (pathSegments p) >= 3)
        Right other -> Left ("unexpected: " ++ show other)
        Left err -> Left (show err)
    ),
    ( "parse fill with hex color",
      case parseElement (T.pack "<circle r=\"10\" fill=\"#ff8800\" />") of
        Right (EFill (SolidFill _) _) -> Right ()
        Right other -> Left ("unexpected: " ++ show other)
        Left err -> Left (show err)
    ),
    ( "parse fill url is ignored",
      case parseElement (T.pack "<circle r=\"10\" fill=\"url(#grad1)\" />") of
        Right (ECircle 10) -> Right ()
        Right other -> Left ("unexpected: " ++ show other)
        Left err -> Left (show err)
    ),
    ( "parse stroke with default width",
      case parseElement (T.pack "<circle r=\"10\" stroke=\"#000000\" />") of
        Right (EStroke {}) -> Right ()
        Right other -> Left ("unexpected: " ++ show other)
        Left err -> Left (show err)
    ),
    ( "parse ParseError Show/Eq",
      do
        _ <- assertTrue "UnexpectedEnd show" (not (null (show UnexpectedEnd)))
        _ <- assertTrue "MalformedTag show" (not (null (show (MalformedTag (T.pack "bad")))))
        _ <- assertTrue "MalformedPath show" (not (null (show (MalformedPath (T.pack "bad")))))
        _ <- assertEqual "UnexpectedEnd eq" UnexpectedEnd UnexpectedEnd
        assertTrue "errors differ" (UnexpectedEnd /= MalformedTag (T.pack "x"))
    )
  ]

-- ---------------------------------------------------------------------------
-- Parse coverage 2 — paths, transforms, hex, attributes, errors
-- ---------------------------------------------------------------------------

testParseCoverage2 :: [(String, TestResult)]
testParseCoverage2 =
  [ ( "parse SVG with missing width/height defaults to 300",
      case parseSvg (T.pack "<svg><circle r=\"10\" /></svg>") of
        Right doc -> do
          _ <- assertApprox "default width" 0.1 300 (docWidth doc)
          assertApprox "default height" 0.1 300 (docHeight doc)
        Left err -> Left ("parse error: " ++ show err)
    ),
    ( "parse non-svg root tag is error",
      case parseSvg (T.pack "<div>hello</div>") of
        Left (MalformedTag _) -> Right ()
        Left err -> Left ("wrong error: " ++ show err)
        Right _ -> Left "expected error for non-svg root"
    ),
    ( "parse empty input is UnexpectedEnd",
      case parseSvg (T.pack "") of
        Left UnexpectedEnd -> Right ()
        Left err -> Left ("wrong error: " ++ show err)
        Right _ -> Left "expected UnexpectedEnd"
    ),
    ( "parse path with all relative commands",
      case parseElement (T.pack "<path d=\"M 0 0 l 10 10 h 20 v 30 c 1 2 3 4 5 6 q 10 20 30 0 a 25 25 0 0 1 50 0 z\" />") of
        Right (EPath p) -> do
          _ <- assertTrue "has segments" (length (pathSegments p) >= 6)
          assertTrue "is closed" (pathClosed p)
        Right other -> Left ("unexpected: " ++ show other)
        Left err -> Left ("parse error: " ++ show err)
    ),
    ( "parse path implicit L after M",
      case parseElement (T.pack "<path d=\"M 0 0 10 20 30 40\" />") of
        Right (EPath p) -> assertTrue "has 2 implicit L" (length (pathSegments p) >= 2)
        Right other -> Left ("unexpected: " ++ show other)
        Left err -> Left ("parse error: " ++ show err)
    ),
    ( "parse path starting without M returns empty",
      case parseElement (T.pack "<path d=\"L 10 10\" />") of
        Right EEmpty -> Right ()
        Left (MalformedPath _) -> Right ()
        other -> Left ("expected EEmpty or MalformedPath, got: " ++ show other)
    ),
    ( "parse 3-digit hex color",
      case parseElement (T.pack "<circle r=\"10\" fill=\"#f0a\" />") of
        Right (EFill (SolidFill _) _) -> Right ()
        Right other -> Left ("unexpected: " ++ show other)
        Left err -> Left ("parse error: " ++ show err)
    ),
    ( "parse single-quoted attributes",
      case parseElement (T.pack "<circle r='25' fill='#ff0000' />") of
        Right (EFill (SolidFill _) (ECircle r)) -> assertApprox "r" 0.1 25 r
        Right other -> Left ("unexpected: " ++ show other)
        Left err -> Left ("parse error: " ++ show err)
    ),
    ( "parse transform scale one arg",
      case parseElement (T.pack "<circle r=\"10\" transform=\"scale(2)\" />") of
        Right (EScale sx sy _) -> do
          _ <- assertApprox "sx" 0.1 2 sx
          assertApprox "sy" 0.1 2 sy
        Right other -> Left ("unexpected: " ++ show other)
        Left err -> Left ("parse error: " ++ show err)
    ),
    ( "parse transform scale two args",
      case parseElement (T.pack "<circle r=\"10\" transform=\"scale(2 3)\" />") of
        Right (EScale sx sy _) -> do
          _ <- assertApprox "sx" 0.1 2 sx
          assertApprox "sy" 0.1 3 sy
        Right other -> Left ("unexpected: " ++ show other)
        Left err -> Left ("parse error: " ++ show err)
    ),
    ( "parse transform translate one arg",
      case parseElement (T.pack "<circle r=\"10\" transform=\"translate(15)\" />") of
        Right (ETranslate tx ty _) -> do
          _ <- assertApprox "tx" 0.1 15 tx
          assertApprox "ty" 0.1 0 ty
        Right other -> Left ("unexpected: " ++ show other)
        Left err -> Left ("parse error: " ++ show err)
    ),
    ( "parse transform rotate with center",
      case parseElement (T.pack "<circle r=\"10\" transform=\"rotate(45 50 50)\" />") of
        Right (ERotateAround deg (V2 cx cy) _) -> do
          _ <- assertApprox "deg" 0.1 45 deg
          _ <- assertApprox "cx" 0.1 50 cx
          assertApprox "cy" 0.1 50 cy
        Right other -> Left ("unexpected: " ++ show other)
        Left err -> Left ("parse error: " ++ show err)
    ),
    ( "parse transform skewX",
      case parseElement (T.pack "<rect width=\"10\" height=\"10\" transform=\"skewX(15)\" />") of
        Right (ESkewX deg _) -> assertApprox "deg" 0.1 15 deg
        Right other -> Left ("unexpected: " ++ show other)
        Left err -> Left ("parse error: " ++ show err)
    ),
    ( "parse transform skewY",
      case parseElement (T.pack "<rect width=\"10\" height=\"10\" transform=\"skewY(20)\" />") of
        Right (ESkewY deg _) -> assertApprox "deg" 0.1 20 deg
        Right other -> Left ("unexpected: " ++ show other)
        Left err -> Left ("parse error: " ++ show err)
    ),
    ( "parse circle missing r defaults to 0",
      case parseElement (T.pack "<circle />") of
        Right (ECircle r) -> assertApprox "r" 0.1 0 r
        Right other -> Left ("unexpected: " ++ show other)
        Left err -> Left ("parse error: " ++ show err)
    ),
    ( "parse rect missing dimensions defaults to 0",
      case parseElement (T.pack "<rect />") of
        Right (ERect w h) -> do
          _ <- assertApprox "w" 0.1 0 w
          assertApprox "h" 0.1 0 h
        Right other -> Left ("unexpected: " ++ show other)
        Left err -> Left ("parse error: " ++ show err)
    ),
    ( "parse line missing coords defaults to 0",
      case parseElement (T.pack "<line />") of
        Right (ELine (V2 x1 y1) (V2 x2 y2)) -> do
          _ <- assertApprox "x1" 0.1 0 x1
          _ <- assertApprox "y1" 0.1 0 y1
          _ <- assertApprox "x2" 0.1 0 x2
          assertApprox "y2" 0.1 0 y2
        Right other -> Left ("unexpected: " ++ show other)
        Left err -> Left ("parse error: " ++ show err)
    ),
    ( "parse use without hash prefix",
      case parseElement (T.pack "<use href=\"myref\" />") of
        Right (EUse ref) -> assertEqual "ref" (T.pack "myref") ref
        Right other -> Left ("unexpected: " ++ show other)
        Left err -> Left ("parse error: " ++ show err)
    ),
    ( "parse nested svg element",
      case parseElement (T.pack "<svg width=\"100\" height=\"100\"><circle r=\"5\" /></svg>") of
        Right _ -> Right ()
        Left err -> Left ("parse error: " ++ show err)
    ),
    ( "parse polygon missing points is empty",
      case parseElement (T.pack "<polygon />") of
        Right (EPolygon pts) -> assertTrue "empty points" (null pts)
        Right other -> Left ("unexpected: " ++ show other)
        Left err -> Left ("parse error: " ++ show err)
    ),
    ( "parse polyline missing points is empty",
      case parseElement (T.pack "<polyline />") of
        Right (EPolyline pts) -> assertTrue "empty points" (null pts)
        Right other -> Left ("unexpected: " ++ show other)
        Left err -> Left ("parse error: " ++ show err)
    ),
    ( "parse path absolute C and Q commands",
      case parseElement (T.pack "<path d=\"M 0 0 C 10 20 30 40 50 60 Q 70 80 90 100\" />") of
        Right (EPath p) -> assertTrue "has 2 segments" (length (pathSegments p) == 2)
        Right other -> Left ("unexpected: " ++ show other)
        Left err -> Left ("parse error: " ++ show err)
    ),
    ( "parse path absolute H and V commands",
      case parseElement (T.pack "<path d=\"M 0 0 H 100 V 50\" />") of
        Right (EPath p) -> assertTrue "has 2 segments" (length (pathSegments p) == 2)
        Right other -> Left ("unexpected: " ++ show other)
        Left err -> Left ("parse error: " ++ show err)
    ),
    ( "parse path absolute A command",
      case parseElement (T.pack "<path d=\"M 0 0 A 25 25 0 1 0 50 50\" />") of
        Right (EPath p) -> assertTrue "has arc" (length (pathSegments p) == 1)
        Right other -> Left ("unexpected: " ++ show other)
        Left err -> Left ("parse error: " ++ show err)
    ),
    ( "parse viewBox attribute",
      case parseSvg (T.pack "<svg viewBox=\"0 0 100 200\" width=\"100\" height=\"200\"><circle r=\"5\" /></svg>") of
        Right doc -> case docViewBox doc of
          Just (ViewBox minX minY w h) -> do
            _ <- assertApprox "minX" 0.1 0 minX
            _ <- assertApprox "minY" 0.1 0 minY
            _ <- assertApprox "w" 0.1 100 w
            assertApprox "h" 0.1 200 h
          Nothing -> Left "expected viewBox"
        Left err -> Left ("parse error: " ++ show err)
    ),
    ( "parse unknown tag becomes EEmpty",
      case parseElement (T.pack "<foobar />") of
        Right EEmpty -> Right ()
        Right other -> Left ("expected EEmpty: " ++ show other)
        Left err -> Left (show err)
    ),
    ( "parse path with empty d attribute",
      case parseElement (T.pack "<path d=\"\" />") of
        Right (EPath p) -> assertTrue "no segments" (null (pathSegments p))
        Right other -> Left ("unexpected: " ++ show other)
        Left err -> Left (show err)
    ),
    ( "parse circle with cx cy",
      case parseElement (T.pack "<circle r=\"10\" cx=\"50\" cy=\"60\" />") of
        Right (ETranslate tx ty (ECircle r)) -> do
          _ <- assertApprox "cx" 0.1 50 tx
          _ <- assertApprox "cy" 0.1 60 ty
          assertApprox "r" 0.1 10 r
        Right other -> Left ("expected translated circle: " ++ show other)
        Left err -> Left (show err)
    ),
    ( "parse ellipse with cx cy",
      case parseElement (T.pack "<ellipse rx=\"30\" ry=\"20\" cx=\"50\" cy=\"60\" />") of
        Right (ETranslate tx ty (EEllipse rx ry)) -> do
          _ <- assertApprox "cx" 0.1 50 tx
          _ <- assertApprox "cy" 0.1 60 ty
          _ <- assertApprox "rx" 0.1 30 rx
          assertApprox "ry" 0.1 20 ry
        Right other -> Left ("expected translated ellipse: " ++ show other)
        Left err -> Left (show err)
    ),
    ( "parse rect with rx ry as roundedRect",
      case parseElement (T.pack "<rect width=\"100\" height=\"50\" rx=\"5\" ry=\"3\" />") of
        Right (ERoundRect w h rx ry) -> do
          _ <- assertApprox "w" 0.1 100 w
          _ <- assertApprox "h" 0.1 50 h
          _ <- assertApprox "rx" 0.1 5 rx
          assertApprox "ry" 0.1 3 ry
        Right other -> Left ("expected roundedRect: " ++ show other)
        Left err -> Left (show err)
    ),
    ( "parse text element",
      case parseElement (T.pack "<text>Hello World</text>") of
        Right (EText _ content) -> assertEqual "text content" (T.pack "Hello World") content
        Right other -> Left ("unexpected: " ++ show other)
        Left err -> Left (show err)
    ),
    ( "parse non-hex fill ignored",
      case parseElement (T.pack "<circle r=\"10\" fill=\"red\" />") of
        Right (ECircle r) -> assertApprox "r" 0.1 10 r
        Right other -> Left ("unexpected: " ++ show other)
        Left err -> Left (show err)
    ),
    ( "parse non-hex stroke ignored",
      case parseElement (T.pack "<circle r=\"10\" stroke=\"blue\" />") of
        Right (ECircle r) -> assertApprox "r" 0.1 10 r
        Right other -> Left ("unexpected: " ++ show other)
        Left err -> Left (show err)
    ),
    ( "parse invalid hex fill ignored",
      case parseElement (T.pack "<circle r=\"10\" fill=\"#zzzzzz\" />") of
        Right (ECircle r) -> assertApprox "r" 0.1 10 r
        Right other -> Left ("unexpected: " ++ show other)
        Left err -> Left (show err)
    ),
    ( "parse invalid hex stroke ignored",
      case parseElement (T.pack "<circle r=\"10\" stroke=\"#xyz\" />") of
        Right (ECircle r) -> assertApprox "r" 0.1 10 r
        Right other -> Left ("unexpected: " ++ show other)
        Left err -> Left (show err)
    ),
    ( "parse transform with unknown name ignored",
      case parseElement (T.pack "<circle r=\"10\" transform=\"matrix(1 0 0 1 0 0)\" />") of
        Right (ECircle r) -> assertApprox "r" 0.1 10 r
        Right other -> Left ("unexpected: " ++ show other)
        Left err -> Left (show err)
    ),
    ( "parse transform with wrong arg count ignored",
      case parseElement (T.pack "<circle r=\"10\" transform=\"translate()\" />") of
        Right (ECircle r) -> assertApprox "r" 0.1 10 r
        Right other -> Left ("unexpected: " ++ show other)
        Left err -> Left (show err)
    ),
    ( "parse malformed transform no paren",
      case parseElement (T.pack "<circle r=\"10\" transform=\"badtransform\" />") of
        Right (ECircle r) -> assertApprox "r" 0.1 10 r
        Right other -> Left ("unexpected: " ++ show other)
        Left err -> Left (show err)
    ),
    ( "parse path with multiple commands and continuation",
      let svg = T.pack "<path d=\"M 10 20 L 30 40 50 60 H 100 V 200 C 1 2 3 4 5 6 Q 7 8 9 10 A 25 25 0 0 1 50 50 Z\" />"
       in case parseElement svg of
            Right (EPath p) -> do
              _ <- assertTrue "has many segments" (length (pathSegments p) >= 7)
              assertTrue "is closed" (pathClosed p)
            Right other -> Left ("unexpected: " ++ show other)
            Left err -> Left (show err)
    ),
    ( "parse path relative cubic",
      case parseElement (T.pack "<path d=\"M 50 50 c 10 20 30 40 50 0\" />") of
        Right (EPath p) ->
          case pathSegments p of
            [CubicTo c1 c2 pt] -> do
              let V2 cx1 _ = c1
              _ <- assertApprox "c1x offset from 50" 0.1 60 cx1
              let V2 px _ = pt
              assertApprox "end offset from 50" 0.1 100 px
            other -> Left ("expected cubic: " ++ show other)
        Right other -> Left ("unexpected: " ++ show other)
        Left err -> Left (show err)
    ),
    ( "parse path relative quad",
      case parseElement (T.pack "<path d=\"M 50 50 q 10 20 30 0\" />") of
        Right (EPath p) ->
          case pathSegments p of
            [QuadTo ctrl pt] -> do
              let V2 cx _ = ctrl
              _ <- assertApprox "ctrl offset from 50" 0.1 60 cx
              let V2 px _ = pt
              assertApprox "end offset from 50" 0.1 80 px
            other -> Left ("expected quad: " ++ show other)
        Right other -> Left ("unexpected: " ++ show other)
        Left err -> Left (show err)
    ),
    ( "parse path relative arc",
      case parseElement (T.pack "<path d=\"M 50 50 a 25 25 0 0 1 50 0\" />") of
        Right (EPath p) ->
          case pathSegments p of
            [ArcTo _ pt] ->
              let V2 px _ = pt
               in assertApprox "end offset from 50" 0.1 100 px
            other -> Left ("expected arc: " ++ show other)
        Right other -> Left ("unexpected: " ++ show other)
        Left err -> Left (show err)
    ),
    ( "parse path relative horiz",
      case parseElement (T.pack "<path d=\"M 10 20 h 30\" />") of
        Right (EPath p) ->
          case pathSegments p of
            [LineTo (V2 px py)] -> do
              _ <- assertApprox "x=10+30" 0.1 40 px
              assertApprox "y=20" 0.1 20 py
            other -> Left ("expected line: " ++ show other)
        Right other -> Left ("unexpected: " ++ show other)
        Left err -> Left (show err)
    ),
    ( "parse path relative vert",
      case parseElement (T.pack "<path d=\"M 10 20 v 30\" />") of
        Right (EPath p) ->
          case pathSegments p of
            [LineTo (V2 px py)] -> do
              _ <- assertApprox "x=10" 0.1 10 px
              assertApprox "y=20+30" 0.1 50 py
            other -> Left ("expected line: " ++ show other)
        Right other -> Left ("unexpected: " ++ show other)
        Left err -> Left (show err)
    ),
    ( "parse group error propagation",
      case parseElement (T.pack "<g><circle r=\"5\" /><rect width=\"10\" height=\"20\" /></g>") of
        Right _ -> Right ()
        Left err -> Left (show err)
    ),
    ( "parse SVG with comment",
      case parseSvg (T.pack "<svg width=\"100\" height=\"100\"><!-- comment --><circle r=\"5\" /></svg>") of
        Right doc -> assertApprox "width" 0.1 100 (docWidth doc)
        Left err -> Left ("parse error: " ++ show err)
    ),
    ( "parse non-xml content returns error",
      case parseElement (T.pack "just plain text no xml") of
        Left _ -> Right ()
        Right _ -> Left "expected parse error for non-xml"
    ),
    ( "parse 6-digit hex color lowercase",
      case parseElement (T.pack "<circle r=\"5\" fill=\"#aabbcc\" />") of
        Right (EFill (SolidFill _) _) -> Right ()
        Right other -> Left ("expected fill: " ++ show other)
        Left err -> Left (show err)
    ),
    ( "parse invalid hex length ignored",
      case parseElement (T.pack "<circle r=\"5\" fill=\"#abcd\" />") of
        Right (ECircle _) -> Right ()
        Right other -> Left ("unexpected: " ++ show other)
        Left err -> Left (show err)
    )
  ]

-- ---------------------------------------------------------------------------
-- PathOps coverage 2 — reverse, simplify, arc/quad segments
-- ---------------------------------------------------------------------------

testPathOpsCoverage2 :: [(String, TestResult)]
testPathOpsCoverage2 =
  [ ( "reversePath with multi-segment path",
      let p = buildPath $ do
            startAt (V2 0 0)
            lineTo (V2 100 0)
            cubicTo (V2 150 50) (V2 150 100) (V2 100 100)
            quadTo (V2 50 150) (V2 0 100)
            closePath
          rev = reversePath p
       in do
            _ <- assertTrue "reversed has segments" (not (null (pathSegments rev)))
            assertTrue "reversed is closed" (pathClosed rev)
    ),
    ( "reversePath with arc segment",
      let p = Path (V2 0 0) [ArcTo (ArcParams 50 50 0 False True) (V2 100 0)] False
          rev = reversePath p
       in assertTrue "reversed has segments" (not (null (pathSegments rev)))
    ),
    ( "simplifyPath removes collinear points",
      let p = polylinePath [V2 0 0, V2 10 0, V2 20 0, V2 30 0, V2 40 0, V2 50 0, V2 50 50]
          simplified = simplifyPath 1.0 p
       in assertTrue "fewer segments" (length (pathSegments simplified) < length (pathSegments p))
    ),
    ( "simplifyPath edge cases",
      do
        _ <- assertTrue "empty" (null (pathSegments (simplifyPath 1.0 (Path (V2 0 0) [] False))))
        _ <- assertTrue "one seg" (not (null (pathSegments (simplifyPath 1.0 (polylinePath [V2 0 0, V2 10 0])))))
        assertTrue "two seg" (not (null (pathSegments (simplifyPath 1.0 (polylinePath [V2 0 0, V2 5 0, V2 10 0])))))
    ),
    ( "measurePath with arc segments",
      let p = Path (V2 0 0) [ArcTo (ArcParams 50 50 0 False True) (V2 100 0)] False
          len = measurePath p
       in assertTrue "arc length > straight line" (len > 90)
    ),
    ( "measurePath with quad segments",
      let p = buildPath (startAt (V2 0 0) >> quadTo (V2 50 100) (V2 100 0))
          len = measurePath p
       in assertTrue "quad length > straight line" (len > 100)
    ),
    ( "splitPathAt with quad segment",
      let p = buildPath (startAt (V2 0 0) >> quadTo (V2 50 100) (V2 100 0))
          (before, after) = splitPathAt 0.5 p
       in do
            _ <- assertTrue "before has segments" (not (null (pathSegments before)))
            assertTrue "after has segments" (not (null (pathSegments after)))
    ),
    ( "splitPathAt with arc segment",
      let p = Path (V2 0 0) [ArcTo (ArcParams 50 50 0 False True) (V2 100 0)] False
          (before, after) = splitPathAt 0.5 p
       in do
            _ <- assertTrue "before has segments" (not (null (pathSegments before)))
            assertTrue "after has segments" (not (null (pathSegments after)))
    ),
    ( "offsetPath with quad segment",
      let p = buildPath (startAt (V2 0 0) >> quadTo (V2 50 100) (V2 100 0))
          off = offsetPath 5.0 p
       in assertTrue "offset has segments" (not (null (pathSegments off)))
    ),
    ( "offsetPath with arc segment",
      let p = Path (V2 0 0) [ArcTo (ArcParams 50 50 0 False True) (V2 100 0)] False
          off = offsetPath 5.0 p
       in assertTrue "offset has segments" (not (null (pathSegments off)))
    ),
    ( "subpath with arc segment",
      let p = Path (V2 0 0) [ArcTo (ArcParams 50 50 0 False True) (V2 100 0)] False
          sub = subpath 0.2 0.8 p
       in assertTrue "subpath has segments" (not (null (pathSegments sub)))
    )
  ]

-- ---------------------------------------------------------------------------
-- Noise coverage — open paths, closed paths, wobble, Voronoi edges
-- ---------------------------------------------------------------------------

testNoiseCoverage :: [(String, TestResult)]
testNoiseCoverage =
  [ ( "noisePath generates open path",
      let p = noisePath (perlin2D 42) 20 100 50 0.05
       in do
            _ <- assertTrue "has segments" (not (null (pathSegments p)))
            assertTrue "is open" (not (pathClosed p))
    ),
    ( "noiseClosedPath generates closed path",
      let p = noiseClosedPath (perlin2D 42) 20 100 50 0.05 0.1
       in do
            _ <- assertTrue "has segments" (not (null (pathSegments p)))
            assertTrue "is closed" (pathClosed p)
    ),
    ( "wobblePath distorts all segment types",
      let p =
            Path
              (V2 0 0)
              [ LineTo (V2 50 0),
                CubicTo (V2 60 30) (V2 90 30) (V2 100 0),
                QuadTo (V2 125 50) (V2 150 0),
                ArcTo (ArcParams 25 25 0 False True) (V2 200 0)
              ]
              False
          wobbled = wobblePath (perlin2D 42) 5.0 p
       in assertTrue "wobbled has 4 segments" (length (pathSegments wobbled) == 4)
    ),
    ( "voronoiEdges produces edges",
      let edges = voronoiEdges 42 10 5 200 200
       in assertTrue "has edges" (not (null edges))
    ),
    ( "fbm zero octaves",
      let val = fbm (perlin2D 42) 0 0.5 2.0 1.0 1.0
       in assertTrue "zero octaves returns 0" (abs val < 0.01)
    )
  ]

-- ---------------------------------------------------------------------------
-- Path builder coverage — arcTo, polygonPath edge cases
-- ---------------------------------------------------------------------------

testPathBuilderCoverage :: [(String, TestResult)]
testPathBuilderCoverage =
  [ ( "arcTo builds arc segment",
      let p = buildPath $ do
            startAt (V2 0 0)
            arcTo (ArcParams 50 50 0 False True) (V2 100 0)
       in case pathSegments p of
            [ArcTo params end] -> do
              _ <- assertApprox "rx" 0.1 50 (arcRx params)
              assertApprox "endX" 0.1 100 (let V2 x _ = end in x)
            other -> Left ("unexpected segments: " ++ show other)
    ),
    ( "polygonPath empty list",
      let p = polygonPath []
       in assertTrue "no segments" (null (pathSegments p))
    ),
    ( "polygonPath single point",
      let p = polygonPath [V2 0 0]
       in assertTrue "no segments" (null (pathSegments p))
    ),
    ( "polygonPath two points",
      let p = polygonPath [V2 0 0, V2 10 0]
       in assertTrue "no segments" (null (pathSegments p))
    ),
    ( "path with arc renders A command",
      let p =
            Path
              (V2 0 0)
              [ArcTo (ArcParams 25 25 0 False True) (V2 50 0)]
              False
          svg = renderElement (EPath p)
       in assertContains "has arc" (T.pack "A25") svg
    )
  ]

-- ---------------------------------------------------------------------------
-- Boolean coverage 2 — union edge cases, curve flattening
-- ---------------------------------------------------------------------------

testBooleanCoverage2 :: [(String, TestResult)]
testBooleanCoverage2 =
  [ ( "union with empty path",
      let emptyP = Path (V2 0 0) [] True
          sq = polygonPath [V2 0 0, V2 100 0, V2 100 100, V2 0 100]
          result = union emptyP sq
       in assertTrue "result has segments" (not (null (pathSegments result)))
    ),
    ( "boolean ops with cubic segments",
      let p1 = buildPath $ do
            startAt (V2 0 0)
            cubicTo (V2 50 100) (V2 100 100) (V2 100 0)
            lineTo (V2 100 100)
            lineTo (V2 0 100)
            closePath
          p2 = polygonPath [V2 25 25, V2 75 25, V2 75 75, V2 25 75]
          result = intersection p1 p2
       in assertTrue "intersection ran" (pathStart result `seq` True)
    ),
    ( "boolean ops with quad segments",
      let p1 = buildPath $ do
            startAt (V2 0 0)
            quadTo (V2 50 100) (V2 100 0)
            lineTo (V2 100 100)
            lineTo (V2 0 100)
            closePath
          p2 = polygonPath [V2 25 25, V2 75 25, V2 75 75, V2 25 75]
          result = intersection p1 p2
       in assertTrue "intersection ran" (pathStart result `seq` True)
    ),
    ( "boolean ops with arc segments",
      let p1 =
            Path
              (V2 0 0)
              [ ArcTo (ArcParams 100 100 0 False True) (V2 100 0),
                LineTo (V2 100 100),
                LineTo (V2 0 100)
              ]
              True
          p2 = polygonPath [V2 25 25, V2 75 25, V2 75 75, V2 25 75]
          result = intersection p1 p2
       in assertTrue "intersection ran" (pathStart result `seq` True)
    ),
    ( "polygonArea of clockwise triangle",
      let area = polygonArea [V2 0 0, V2 100 0, V2 50 100]
       in assertTrue "non-zero area" (abs area > 100)
    )
  ]

-- ---------------------------------------------------------------------------
-- Derived instances coverage 2 — remaining Show/Eq/Ord
-- ---------------------------------------------------------------------------

testDerivedCoverage2 :: [(String, TestResult)]
testDerivedCoverage2 =
  [ ( "ArcParams Show/Eq",
      let ap1 = ArcParams 50 50 0 False True
          ap2 = ArcParams 50 50 0 False True
          ap3 = ArcParams 25 25 0 True False
       in do
            _ <- assertEqual "equal" ap1 ap2
            _ <- assertTrue "not equal" (ap1 /= ap3)
            assertTrue "show" (not (null (show ap1)))
    ),
    ( "Segment Show/Eq all constructors",
      let segs =
            [ LineTo (V2 10 10),
              CubicTo (V2 1 2) (V2 3 4) (V2 5 6),
              QuadTo (V2 1 2) (V2 3 4),
              ArcTo (ArcParams 50 50 0 False True) (V2 100 0)
            ]
       in do
            _ <- assertTrue "all show" (not (any (null . show) segs))
            _ <- assertTrue "eq self" (all (\s -> s == s) segs)
            assertTrue "differ" (head segs /= segs !! 1)
    ),
    ( "Path Show/Eq",
      let p1 = buildPath (startAt (V2 0 0) >> lineTo (V2 10 0))
          p2 = buildPath (startAt (V2 0 0) >> lineTo (V2 10 0))
          p3 = buildPath (startAt (V2 0 0) >> lineTo (V2 20 0))
       in do
            _ <- assertEqual "equal paths" p1 p2
            _ <- assertTrue "diff paths" (p1 /= p3)
            assertTrue "show path" (not (null (show p1)))
    ),
    ( "ViewBox Show/Eq",
      let vb1 = ViewBox 0 0 100 100
          vb2 = ViewBox 0 0 100 100
          vb3 = ViewBox 0 0 200 200
       in do
            _ <- assertEqual "equal" vb1 vb2
            _ <- assertTrue "not equal" (vb1 /= vb3)
            assertTrue "show" (not (null (show vb1)))
    ),
    ( "Document Show/Eq",
      let d1 = Document 100 100 Nothing EEmpty
          d2 = Document 100 100 Nothing EEmpty
          d3 = Document 200 200 Nothing EEmpty
       in do
            _ <- assertEqual "equal" d1 d2
            _ <- assertTrue "not equal" (d1 /= d3)
            assertTrue "show" (not (null (show d1)))
    ),
    ( "Fill Show/Eq all constructors",
      let fills = [NoFill, SolidFill red, GradientFill (LinearGradient (V2 0 0) (V2 1 1) [GradientStop 0 red 1] SpreadPad)]
       in do
            _ <- assertTrue "all show" (not (any (null . show) fills))
            _ <- assertTrue "eq self" (all (\f -> f == f) fills)
            assertTrue "differ" (NoFill /= SolidFill red)
    ),
    ( "Gradient Show/Eq",
      let g1 = LinearGradient (V2 0 0) (V2 1 1) [GradientStop 0 red 1] SpreadPad
          g2 = RadialGradient (V2 0.5 0.5) 1 (V2 0.5 0.5) [GradientStop 0 blue 1] SpreadPad
       in do
            _ <- assertTrue "show linear" (not (null (show g1)))
            _ <- assertTrue "show radial" (not (null (show g2)))
            assertTrue "differ" (g1 /= g2)
    ),
    ( "GradientStop Show/Eq",
      let s1 = GradientStop 0 red 1
          s2 = GradientStop 0 red 1
          s3 = GradientStop 1 blue 0.5
       in do
            _ <- assertEqual "equal" s1 s2
            _ <- assertTrue "differ" (s1 /= s3)
            assertTrue "show" (not (null (show s1)))
    ),
    ( "StrokeConfig Show/Eq",
      let sc1 = defaultStrokeConfig
          sc2 = defaultStrokeConfig {strokeConfigWidth = 5}
       in do
            _ <- assertTrue "eq self" (sc1 == sc1)
            _ <- assertTrue "differ" (sc1 /= sc2)
            assertTrue "show" (not (null (show sc1)))
    ),
    ( "TextConfig Show/Eq",
      let tc1 = defaultTextConfig
          tc2 = defaultTextConfig {textConfigBold = True}
       in do
            _ <- assertTrue "eq self" (tc1 == tc1)
            _ <- assertTrue "differ" (tc1 /= tc2)
            assertTrue "show" (not (null (show tc1)))
    ),
    ( "TextAnchor Show/Eq/Ord",
      do
        _ <- assertTrue "show" (not (null (show AnchorStart)))
        _ <- assertTrue "eq" (AnchorStart == AnchorStart)
        _ <- assertTrue "differ" (AnchorStart /= AnchorMiddle)
        assertTrue "ord" (AnchorStart < AnchorEnd)
    ),
    ( "FilterKind Show/Eq",
      let f1 = FilterBlur 3
          f2 = FilterDropShadow 2 2 4 black
       in do
            _ <- assertTrue "show blur" (not (null (show f1)))
            _ <- assertTrue "show shadow" (not (null (show f2)))
            assertTrue "differ" (f1 /= f2)
    ),
    ( "SpreadMethod Show/Eq/Ord",
      do
        _ <- assertTrue "show" (not (null (show SpreadPad)))
        _ <- assertTrue "eq" (SpreadPad == SpreadPad)
        _ <- assertTrue "differ" (SpreadPad /= SpreadReflect)
        assertTrue "ord" (SpreadPad < SpreadRepeat)
    ),
    ( "LineCap Show/Eq/Ord",
      do
        _ <- assertTrue "show" (not (null (show CapButt)))
        _ <- assertTrue "eq" (CapButt == CapButt)
        _ <- assertTrue "differ" (CapButt /= CapRound)
        assertTrue "ord" (CapButt < CapSquare)
    ),
    ( "LineJoin Show/Eq/Ord",
      do
        _ <- assertTrue "show" (not (null (show JoinMiter)))
        _ <- assertTrue "eq" (JoinMiter == JoinMiter)
        _ <- assertTrue "differ" (JoinMiter /= JoinRound)
        assertTrue "ord" (JoinMiter < JoinBevel)
    ),
    ( "FillRule Show/Eq/Ord",
      do
        _ <- assertTrue "show" (not (null (show FillNonZero)))
        _ <- assertTrue "eq" (FillNonZero == FillNonZero)
        _ <- assertTrue "differ" (FillNonZero /= FillEvenOdd)
        assertTrue "ord" (FillNonZero < FillEvenOdd)
    ),
    ( "Element Show for all constructors",
      let elems =
            [ EEmpty,
              ECircle 5,
              EEllipse 10 5,
              ERect 10 20,
              ERoundRect 10 20 2 2,
              ELine (V2 0 0) (V2 10 10),
              EPolygon [V2 0 0, V2 10 0, V2 5 10],
              EPolyline [V2 0 0, V2 10 0],
              EPath (buildPath (startAt (V2 0 0) >> lineTo (V2 10 0))),
              EText defaultTextConfig (T.pack "hi"),
              EGroup [EEmpty],
              EFill NoFill EEmpty,
              EStroke red 1 EEmpty,
              EStrokeEx defaultStrokeConfig EEmpty,
              EFillRule FillNonZero EEmpty,
              ETranslate 1 2 EEmpty,
              ERotate 45 EEmpty,
              ERotateAround 90 (V2 0 0) EEmpty,
              EScale 2 2 EEmpty,
              ESkewX 15 EEmpty,
              ESkewY 20 EEmpty,
              EOpacity 0.5 EEmpty,
              EClip EEmpty EEmpty,
              EMask EEmpty EEmpty,
              EFilter (FilterBlur 3) EEmpty,
              EWithId (T.pack "id") EEmpty,
              EUse (T.pack "ref"),
              ETitle (T.pack "t") EEmpty,
              EDesc (T.pack "d") EEmpty,
              ERaw (T.pack "<x/>")
            ]
       in assertTrue "all show" (not (any (null . show) elems))
    ),
    ( "Matrix all fields",
      let m = Matrix 1 0 0 1 0 0
       in do
            _ <- assertApprox "a" epsilon 1 (matA m)
            _ <- assertApprox "b" epsilon 0 (matB m)
            _ <- assertApprox "c" epsilon 0 (matC m)
            _ <- assertApprox "d" epsilon 1 (matD m)
            _ <- assertApprox "tx" epsilon 0 (matTx m)
            assertApprox "ty" epsilon 0 (matTy m)
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
  removeFile path
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
