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
import GBVector.Color
  ( Color (..),
    black,
    blue,
    crimson,
    gold,
    green,
    hex,
    lerp,
    red,
    rgb,
    rgb8,
    rgba,
    toHex,
    transparent,
    white,
    withAlpha,
  )
import GBVector.Compose (background, document, documentWithViewBox, empty, group)
import GBVector.Element
  ( Document (..),
    Element (..),
    Fill (..),
    Gradient (..),
    GradientStop (..),
    TextAnchor (..),
    TextConfig (..),
  )
import GBVector.Gradient (evenStops, linearGradient, radialGradient, stop)
import GBVector.Path (buildPath, closePath, cubicTo, lineTo, polygonPath, polylinePath, startAt)
import GBVector.SVG (render, renderElement, writeSvg)
import GBVector.Shape (arc, circle, ellipse, line, polygon, rect, regularPolygon, ring, roundedRect, square, star)
import GBVector.Style (blur, clip, dropShadow, fill, fillNone, opacity, stroke, use, withId)
import GBVector.Text (defaultTextConfig, text, textAt)
import GBVector.Transform (rotate, rotateAround, scale, scaleXY, skewX, skewY, translate)
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
import System.IO (hFlush, stdout)

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
        ++ testElement
        ++ testPath
        ++ testShape
        ++ testTransform
        ++ testStyle
        ++ testCompose
        ++ testSvgRender
        ++ testBezier
        ++ testGradient
        ++ testText
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
-- SVG file output test
-- ---------------------------------------------------------------------------

testSvgFile :: IO [(String, TestResult)]
testSvgFile = do
  let doc = document 200 200 (fill red (circle 50))
      path = "/tmp/gb-vector-test.svg"
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
