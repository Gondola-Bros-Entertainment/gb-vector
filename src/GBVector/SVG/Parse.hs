{-# LANGUAGE OverloadedStrings #-}

-- | Parse SVG text back into an 'Element' tree.
--
-- Supports a core SVG subset: basic shapes (@\<circle\>@, @\<rect\>@,
-- @\<ellipse\>@, @\<line\>@, @\<polygon\>@, @\<polyline\>@), paths
-- (@\<path\>@), groups (@\<g\>@), text (@\<text\>@), and common
-- presentation attributes (@fill@, @stroke@, @opacity@, @transform@).
--
-- This enables round-trip workflows: render an 'Element' to SVG text,
-- then parse it back, manipulate programmatically, and re-export.
module GBVector.SVG.Parse
  ( -- * Parsing
    parseSvg,
    parseElement,

    -- * Errors
    ParseError (..),
  )
where

import Data.Char (isAlpha, isDigit, isHexDigit, isSpace, toLower)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import GBVector.Color (Color (..))
import GBVector.Element
  ( Document (..),
    Element (..),
    Fill (..),
    TextAnchor (..),
    TextConfig (..),
  )
import GBVector.Types
  ( ArcParams (..),
    Path (..),
    Segment (..),
    V2 (..),
    ViewBox (..),
  )

-- ---------------------------------------------------------------------------
-- Errors
-- ---------------------------------------------------------------------------

-- | Errors that can occur during SVG parsing.
data ParseError
  = UnexpectedEnd
  | MalformedTag !Text
  | MalformedPath !Text
  deriving (Show, Eq)

-- ---------------------------------------------------------------------------
-- Public API
-- ---------------------------------------------------------------------------

-- | Parse an SVG document string into a 'Document'.
-- Returns 'Left' with an error or 'Right' with the parsed document.
parseSvg :: Text -> Either ParseError Document
parseSvg input =
  let trimmed = T.strip input
   in case findTag trimmed of
        Nothing -> Left UnexpectedEnd
        Just (tagName, attrs, body, _)
          | tagName == "svg" ->
              let width = readAttrDouble "width" attrs
                  height = readAttrDouble "height" attrs
                  vb = parseViewBoxAttr attrs
               in case parseChildren body of
                    Left err -> Left err
                    Right children ->
                      Right
                        Document
                          { docWidth = fromMaybe defaultDimension width,
                            docHeight = fromMaybe defaultDimension height,
                            docViewBox = vb,
                            docElement = groupChildren children
                          }
          | otherwise -> Left (MalformedTag "expected <svg>")

-- | Parse an SVG fragment (no surrounding @\<svg\>@ tag) into an 'Element'.
parseElement :: Text -> Either ParseError Element
parseElement input =
  let trimmed = T.strip input
   in if T.null trimmed
        then Right EEmpty
        else case parseOneElement trimmed of
          Nothing -> Left UnexpectedEnd
          Just (el, _) -> Right el

-- ---------------------------------------------------------------------------
-- Element Parsing
-- ---------------------------------------------------------------------------

-- | Parse a single element, returning the element and remaining text.
parseOneElement :: Text -> Maybe (Element, Text)
parseOneElement input =
  let trimmed = skipWhitespace input
   in case T.uncons trimmed of
        Nothing -> Nothing
        Just (c, _) | c /= '<' -> Nothing
        Just _
          | T.isPrefixOf "<!--" trimmed -> skipComment trimmed
          | otherwise -> case findTag trimmed of
              Nothing -> Nothing
              Just (tagName, attrs, body, rest) ->
                case parseTagToElement tagName attrs body of
                  Left _ -> Just (EEmpty, rest)
                  Right el -> Just (el, rest)

-- | Parse all child elements from body text.
parseChildren :: Text -> Either ParseError [Element]
parseChildren input = Right (go (skipWhitespace input))
  where
    go remaining = case T.uncons remaining of
      Nothing -> []
      Just (c, _)
        | c /= '<' -> []
        | T.isPrefixOf "</" remaining -> []
        | otherwise -> case parseOneElement remaining of
            Nothing -> []
            Just (el, rest) -> el : go (skipWhitespace rest)

-- | Convert a parsed tag into an Element with presentation attributes applied.
parseTagToElement :: Text -> [(Text, Text)] -> Text -> Either ParseError Element
parseTagToElement tagName attrs body =
  case parseShape tagName attrs body of
    Left err -> Left err
    Right el -> Right (applyAttributes attrs el)

-- | Parse the core shape/element from the tag name.
parseShape :: Text -> [(Text, Text)] -> Text -> Either ParseError Element
parseShape tagName attrs body = case tagName of
  "circle" -> Right (parseCircle attrs)
  "ellipse" -> Right (parseEllipse attrs)
  "rect" -> Right (parseRect attrs)
  "line" -> Right (parseLine attrs)
  "polygon" -> Right (parsePolygonEl attrs)
  "polyline" -> Right (parsePolylineEl attrs)
  "path" -> parsePathEl attrs
  "g" -> parseGroup body
  "text" -> Right (parseTextEl body)
  "use" -> Right (parseUse attrs)
  "svg" ->
    case parseChildren body of
      Left err -> Left err
      Right children -> Right (groupChildren children)
  _ -> Right EEmpty

-- ---------------------------------------------------------------------------
-- Shape Parsers
-- ---------------------------------------------------------------------------

parseCircle :: [(Text, Text)] -> Element
parseCircle attrs =
  let r = fromMaybe 0 (readAttrDouble "r" attrs)
      cx = fromMaybe 0 (readAttrDouble "cx" attrs)
      cy = fromMaybe 0 (readAttrDouble "cy" attrs)
   in if cx == 0 && cy == 0
        then ECircle r
        else ETranslate cx cy (ECircle r)

parseEllipse :: [(Text, Text)] -> Element
parseEllipse attrs =
  let rx = fromMaybe 0 (readAttrDouble "rx" attrs)
      ry = fromMaybe 0 (readAttrDouble "ry" attrs)
      cx = fromMaybe 0 (readAttrDouble "cx" attrs)
      cy = fromMaybe 0 (readAttrDouble "cy" attrs)
   in if cx == 0 && cy == 0
        then EEllipse rx ry
        else ETranslate cx cy (EEllipse rx ry)

parseRect :: [(Text, Text)] -> Element
parseRect attrs =
  let w = fromMaybe 0 (readAttrDouble "width" attrs)
      h = fromMaybe 0 (readAttrDouble "height" attrs)
      rx = fromMaybe 0 (readAttrDouble "rx" attrs)
      ry = fromMaybe 0 (readAttrDouble "ry" attrs)
   in if rx > 0 || ry > 0
        then ERoundRect w h rx ry
        else ERect w h

parseLine :: [(Text, Text)] -> Element
parseLine attrs =
  let x1 = fromMaybe 0 (readAttrDouble "x1" attrs)
      y1 = fromMaybe 0 (readAttrDouble "y1" attrs)
      x2 = fromMaybe 0 (readAttrDouble "x2" attrs)
      y2 = fromMaybe 0 (readAttrDouble "y2" attrs)
   in ELine (V2 x1 y1) (V2 x2 y2)

parsePolygonEl :: [(Text, Text)] -> Element
parsePolygonEl attrs =
  let pts = fromMaybe [] (readAttrPoints "points" attrs)
   in EPolygon pts

parsePolylineEl :: [(Text, Text)] -> Element
parsePolylineEl attrs =
  let pts = fromMaybe [] (readAttrPoints "points" attrs)
   in EPolyline pts

parseTextEl :: Text -> Element
parseTextEl body = EText defaultParseTextConfig (T.strip body)

parseUse :: [(Text, Text)] -> Element
parseUse attrs =
  let href = fromMaybe "" (lookupAttr "href" attrs)
      refId = if T.isPrefixOf "#" href then T.drop 1 href else href
   in EUse refId

parseGroup :: Text -> Either ParseError Element
parseGroup body =
  case parseChildren body of
    Left err -> Left err
    Right children -> Right (groupChildren children)

parsePathEl :: [(Text, Text)] -> Either ParseError Element
parsePathEl attrs =
  case lookupAttr "d" attrs of
    Nothing -> Right (EPath (Path (V2 0 0) [] False))
    Just d -> case parsePathData d of
      Left err -> Left err
      Right path -> Right (EPath path)

-- ---------------------------------------------------------------------------
-- Presentation Attributes
-- ---------------------------------------------------------------------------

-- | Apply fill, stroke, opacity, and transform attributes to an element.
applyAttributes :: [(Text, Text)] -> Element -> Element
applyAttributes attrs =
  applyTransformAttr attrs
    . applyOpacityAttr attrs
    . applyStrokeAttr attrs
    . applyFillAttr attrs

applyFillAttr :: [(Text, Text)] -> Element -> Element
applyFillAttr attrs el =
  case lookupAttr "fill" attrs of
    Nothing -> el
    Just val
      | val == "none" -> EFill NoFill el
      | T.isPrefixOf "url(" val -> el
      | T.isPrefixOf "#" val ->
          case parseHexToColor val of
            Just c -> EFill (SolidFill c) el
            Nothing -> el
      | otherwise -> el

applyStrokeAttr :: [(Text, Text)] -> Element -> Element
applyStrokeAttr attrs el =
  case lookupAttr "stroke" attrs of
    Nothing -> el
    Just val
      | T.isPrefixOf "#" val ->
          case parseHexToColor val of
            Just c ->
              let w = fromMaybe 1 (readAttrDouble "stroke-width" attrs)
               in EStroke c w el
            Nothing -> el
      | otherwise -> el

applyOpacityAttr :: [(Text, Text)] -> Element -> Element
applyOpacityAttr attrs el =
  case readAttrDouble "opacity" attrs of
    Nothing -> el
    Just a -> EOpacity a el

applyTransformAttr :: [(Text, Text)] -> Element -> Element
applyTransformAttr attrs el =
  case lookupAttr "transform" attrs of
    Nothing -> el
    Just val -> applyTransformValue val el

-- | Parse a transform attribute value and wrap the element.
applyTransformValue :: Text -> Element -> Element
applyTransformValue val el =
  let transforms = parseTransformFunctions val
   in foldr ($) el transforms

-- | Parse transform functions from a transform attribute string.
parseTransformFunctions :: Text -> [Element -> Element]
parseTransformFunctions input = go (skipWhitespace input)
  where
    go remaining
      | T.null remaining = []
      | otherwise =
          case parseOneTransform remaining of
            Nothing -> []
            Just (tf, rest) -> tf : go (skipWhitespace rest)

parseOneTransform :: Text -> Maybe (Element -> Element, Text)
parseOneTransform input =
  let trimmed = skipWhitespace input
   in case T.break (== '(') trimmed of
        (_, rest) | T.null rest -> Nothing
        (name, argsRest) ->
          case T.break (== ')') (T.drop 1 argsRest) of
            (_, rest2) | T.null rest2 -> Nothing
            (argStr, afterClose) ->
              let args = parseNumberList argStr
                  remaining = T.drop 1 afterClose
               in Just (makeTransform (T.strip name) args, remaining)

makeTransform :: Text -> [Double] -> Element -> Element
makeTransform name args = case name of
  "translate" -> case args of
    [tx, ty] -> ETranslate tx ty
    [tx] -> ETranslate tx 0
    _ -> id
  "rotate" -> case args of
    [deg] -> ERotate deg
    [deg, cx, cy] -> ERotateAround deg (V2 cx cy)
    _ -> id
  "scale" -> case args of
    [sx, sy] -> EScale sx sy
    [s] -> EScale s s
    _ -> id
  "skewX" -> case args of
    [deg] -> ESkewX deg
    _ -> id
  "skewY" -> case args of
    [deg] -> ESkewY deg
    _ -> id
  _ -> id

-- ---------------------------------------------------------------------------
-- SVG Path Data Parser
-- ---------------------------------------------------------------------------

-- | Parse SVG path data string (the @d@ attribute) into a 'Path'.
parsePathData :: Text -> Either ParseError Path
parsePathData input =
  let trimmed = T.strip input
   in if T.null trimmed
        then Right (Path (V2 0 0) [] False)
        else case parsePathCommands trimmed of
          Left err -> Left err
          Right (start, segs, closed) -> Right (Path start segs closed)

-- | Parse path commands, returning start point, segments, and closed flag.
parsePathCommands :: Text -> Either ParseError (V2, [Segment], Bool)
parsePathCommands input =
  let trimmed = skipWhitespace input
   in case T.uncons trimmed of
        Nothing -> Right (V2 0 0, [], False)
        Just (cmd, rest)
          | cmd == 'M' || cmd == 'm' ->
              case readTwoNumbers (skipWhitespace rest) of
                Nothing -> Left (MalformedPath "expected coordinates after M")
                Just (x, y, remaining) ->
                  let start = V2 x y
                      (segs, closed, _) = parseSegments start remaining
                   in Right (start, segs, closed)
          | otherwise -> Left (MalformedPath "path must start with M")

-- | Parse remaining path segments after the initial M command.
parseSegments :: V2 -> Text -> ([Segment], Bool, Text)
parseSegments currentPt input =
  let trimmed = skipWhitespace input
   in case T.uncons trimmed of
        Nothing -> ([], False, trimmed)
        Just (cmd, rest)
          | cmd == 'Z' || cmd == 'z' -> ([], True, skipWhitespace rest)
          | cmd == 'L' || cmd == 'l' ->
              parseLineSeg cmd currentPt rest
          | cmd == 'H' || cmd == 'h' ->
              parseHorizSeg cmd currentPt rest
          | cmd == 'V' || cmd == 'v' ->
              parseVertSeg cmd currentPt rest
          | cmd == 'C' || cmd == 'c' ->
              parseCubicSeg cmd currentPt rest
          | cmd == 'Q' || cmd == 'q' ->
              parseQuadSeg cmd currentPt rest
          | cmd == 'A' || cmd == 'a' ->
              parseArcSeg cmd currentPt rest
          | isDigit cmd || cmd == '-' || cmd == '.' ->
              parseImplicitLine trimmed
          | otherwise -> ([], False, trimmed)

parseLineSeg :: Char -> V2 -> Text -> ([Segment], Bool, Text)
parseLineSeg cmd currentPt rest =
  case readTwoNumbers (skipWhitespace rest) of
    Nothing -> ([], False, rest)
    Just (x, y, remaining) ->
      let pt = if cmd == 'l' then addV2 currentPt (V2 x y) else V2 x y
          (moreSegs, closed, finalRest) = parseSegments pt remaining
       in (LineTo pt : moreSegs, closed, finalRest)

parseHorizSeg :: Char -> V2 -> Text -> ([Segment], Bool, Text)
parseHorizSeg cmd currentPt rest =
  case readOneNumber (skipWhitespace rest) of
    Nothing -> ([], False, rest)
    Just (x, remaining) ->
      let V2 cx cy = currentPt
          px = if cmd == 'h' then cx + x else x
          pt = V2 px cy
          (moreSegs, closed, finalRest) = parseSegments pt remaining
       in (LineTo pt : moreSegs, closed, finalRest)

parseVertSeg :: Char -> V2 -> Text -> ([Segment], Bool, Text)
parseVertSeg cmd currentPt rest =
  case readOneNumber (skipWhitespace rest) of
    Nothing -> ([], False, rest)
    Just (y, remaining) ->
      let V2 cx cy = currentPt
          py = if cmd == 'v' then cy + y else y
          pt = V2 cx py
          (moreSegs, closed, finalRest) = parseSegments pt remaining
       in (LineTo pt : moreSegs, closed, finalRest)

parseCubicSeg :: Char -> V2 -> Text -> ([Segment], Bool, Text)
parseCubicSeg cmd currentPt rest =
  case readSixNumbers (skipWhitespace rest) of
    Nothing -> ([], False, rest)
    Just (x1, y1, x2, y2, x, y, remaining) ->
      let offset = if cmd == 'c' then currentPt else V2 0 0
          c1 = addV2 offset (V2 x1 y1)
          c2 = addV2 offset (V2 x2 y2)
          pt = addV2 offset (V2 x y)
          (moreSegs, closed, finalRest) = parseSegments pt remaining
       in (CubicTo c1 c2 pt : moreSegs, closed, finalRest)

parseQuadSeg :: Char -> V2 -> Text -> ([Segment], Bool, Text)
parseQuadSeg cmd currentPt rest =
  case readFourNumbers (skipWhitespace rest) of
    Nothing -> ([], False, rest)
    Just (cx, cy, x, y, remaining) ->
      let offset = if cmd == 'q' then currentPt else V2 0 0
          ctrl = addV2 offset (V2 cx cy)
          pt = addV2 offset (V2 x y)
          (moreSegs, closed, finalRest) = parseSegments pt remaining
       in (QuadTo ctrl pt : moreSegs, closed, finalRest)

parseArcSeg :: Char -> V2 -> Text -> ([Segment], Bool, Text)
parseArcSeg cmd currentPt rest =
  case readArcNumbers (skipWhitespace rest) of
    Nothing -> ([], False, rest)
    Just (params, x, y, remaining) ->
      let pt = if cmd == 'a' then addV2 currentPt (V2 x y) else V2 x y
          (moreSegs, closed, finalRest) = parseSegments pt remaining
       in (ArcTo params pt : moreSegs, closed, finalRest)

parseImplicitLine :: Text -> ([Segment], Bool, Text)
parseImplicitLine trimmed =
  case readTwoNumbers trimmed of
    Nothing -> ([], False, trimmed)
    Just (x, y, remaining) ->
      let pt = V2 x y
          (moreSegs, closed, finalRest) = parseSegments pt remaining
       in (LineTo pt : moreSegs, closed, finalRest)

-- ---------------------------------------------------------------------------
-- Number Reading
-- ---------------------------------------------------------------------------

readOneNumber :: Text -> Maybe (Double, Text)
readOneNumber = readDouble . skipSep

readTwoNumbers :: Text -> Maybe (Double, Double, Text)
readTwoNumbers input = do
  (x, r1) <- readOneNumber input
  (y, r2) <- readOneNumber r1
  Just (x, y, r2)

readFourNumbers :: Text -> Maybe (Double, Double, Double, Double, Text)
readFourNumbers input = do
  (a, r1) <- readOneNumber input
  (b, r2) <- readOneNumber r1
  (c, r3) <- readOneNumber r2
  (d, r4) <- readOneNumber r3
  Just (a, b, c, d, r4)

readSixNumbers :: Text -> Maybe (Double, Double, Double, Double, Double, Double, Text)
readSixNumbers input = do
  (a, r1) <- readOneNumber input
  (b, r2) <- readOneNumber r1
  (c, r3) <- readOneNumber r2
  (d, r4) <- readOneNumber r3
  (e, r5) <- readOneNumber r4
  (f, r6) <- readOneNumber r5
  Just (a, b, c, d, e, f, r6)

readArcNumbers :: Text -> Maybe (ArcParams, Double, Double, Text)
readArcNumbers input = do
  (rx, r1) <- readOneNumber input
  (ry, r2) <- readOneNumber r1
  (rot, r3) <- readOneNumber r2
  (la, r4) <- readOneNumber r3
  (sw, r5) <- readOneNumber r4
  (x, r6) <- readOneNumber r5
  (y, r7) <- readOneNumber r6
  Just (ArcParams rx ry rot (la /= 0) (sw /= 0), x, y, r7)

-- | Read a double from the front of text.
readDouble :: Text -> Maybe (Double, Text)
readDouble input =
  let trimmed = skipSep input
   in if T.null trimmed
        then Nothing
        else
          let (numStr, rest) = T.span isNumChar trimmed
           in if T.null numStr
                then Nothing
                else case safeReadDouble (T.unpack numStr) of
                  Nothing -> Nothing
                  Just d -> Just (d, rest)

-- | Parse a comma/space-separated list of numbers.
parseNumberList :: Text -> [Double]
parseNumberList input = go (skipSep input)
  where
    go remaining
      | T.null remaining = []
      | otherwise = case readDouble remaining of
          Nothing -> []
          Just (n, rest) -> n : go (skipSep rest)

-- | Safe read for Double.
safeReadDouble :: String -> Maybe Double
safeReadDouble [] = Nothing
safeReadDouble s = case reads s of
  [(d, "")] -> Just d
  [(d, leftover)] | all isSpace leftover -> Just d
  _ -> Nothing

-- ---------------------------------------------------------------------------
-- Hex Color Parsing
-- ---------------------------------------------------------------------------

-- | Parse a hex color like @#rrggbb@ or @#rgb@ to a 'Color'.
parseHexToColor :: Text -> Maybe Color
parseHexToColor hex =
  let digits = T.drop 1 hex
   in case T.length digits of
        6 ->
          let rr = readHexByte (T.take 2 digits)
              gg = readHexByte (T.take 2 (T.drop 2 digits))
              bb = readHexByte (T.take 2 (T.drop 4 digits))
           in case (rr, gg, bb) of
                (Just r, Just g, Just b) -> Just (Color r g b 1.0)
                _ -> Nothing
        3 ->
          case T.unpack digits of
            [rc, gc, bc] ->
              let r = hexNibbleToDouble rc
                  g = hexNibbleToDouble gc
                  b = hexNibbleToDouble bc
               in case (r, g, b) of
                    (Just rv, Just gv, Just bv) -> Just (Color rv gv bv 1.0)
                    _ -> Nothing
            _ -> Nothing
        _ -> Nothing

-- | Parse a two-character hex string to a Double in [0, 1].
readHexByte :: Text -> Maybe Double
readHexByte t =
  case T.unpack t of
    [h, l] -> do
      hv <- hexDigitValue h
      lv <- hexDigitValue l
      Just (fromIntegral (hv * hexBase + lv) / hexByteMax)
    _ -> Nothing

-- | Parse a single hex character to a Double in [0, 1] (nibble expanded).
hexNibbleToDouble :: Char -> Maybe Double
hexNibbleToDouble c = do
  v <- hexDigitValue c
  Just (fromIntegral (v * hexBase + v) / hexByteMax)

-- | Convert a hex digit character to its integer value.
hexDigitValue :: Char -> Maybe Int
hexDigitValue c
  | isHexDigit c =
      let lc = toLower c
       in if isDigit lc
            then Just (fromEnum lc - fromEnum '0')
            else Just (fromEnum lc - fromEnum 'a' + hexLetterOffset)
  | otherwise = Nothing

-- ---------------------------------------------------------------------------
-- Attribute Helpers
-- ---------------------------------------------------------------------------

lookupAttr :: Text -> [(Text, Text)] -> Maybe Text
lookupAttr _ [] = Nothing
lookupAttr key ((k, v) : rest)
  | k == key = Just v
  | otherwise = lookupAttr key rest

readAttrDouble :: Text -> [(Text, Text)] -> Maybe Double
readAttrDouble key attrs = do
  val <- lookupAttr key attrs
  let stripped = T.takeWhile (\c -> isDigit c || c == '.' || c == '-' || c == 'e' || c == 'E') val
  safeReadDouble (T.unpack stripped)

readAttrPoints :: Text -> [(Text, Text)] -> Maybe [V2]
readAttrPoints key attrs = do
  val <- lookupAttr key attrs
  Just (pairUp (parseNumberList val))

pairUp :: [Double] -> [V2]
pairUp (x : y : rest) = V2 x y : pairUp rest
pairUp _ = []

parseViewBoxAttr :: [(Text, Text)] -> Maybe ViewBox
parseViewBoxAttr attrs = do
  val <- lookupAttr "viewBox" attrs
  case parseNumberList val of
    [minX, minY, w, h] -> Just (ViewBox minX minY w h)
    _ -> Nothing

-- ---------------------------------------------------------------------------
-- XML Tag Parsing
-- ---------------------------------------------------------------------------

-- | Find and parse the next XML tag.
-- Returns (tagName, attributes, body, remainingText).
findTag :: Text -> Maybe (Text, [(Text, Text)], Text, Text)
findTag input =
  let trimmed = skipWhitespace input
   in case T.uncons trimmed of
        Just ('<', rest) ->
          let (tagAndAttrs, afterOpen) = T.break (\c -> c == '>' || c == '/') rest
           in if T.null afterOpen
                then Nothing
                else
                  let (tagName, attrStr) = T.break isSpace (T.strip tagAndAttrs)
                      attrs = parseAttributeList (T.strip attrStr)
                   in if T.isPrefixOf "/>" afterOpen
                        then Just (tagName, attrs, T.empty, T.drop 2 afterOpen)
                        else
                          let bodyStart = T.drop 1 afterOpen
                           in case findCloseTag tagName bodyStart of
                                Just (body, remaining) -> Just (tagName, attrs, body, remaining)
                                Nothing -> Just (tagName, attrs, bodyStart, T.empty)
        _ -> Nothing

-- | Find the matching close tag, handling nesting.
findCloseTag :: Text -> Text -> Maybe (Text, Text)
findCloseTag tagName = findCloseHelper tagName openStr closeStr 1 T.empty
  where
    closeStr = "</" <> tagName <> ">"
    openStr = "<" <> tagName

findCloseHelper :: Text -> Text -> Text -> Int -> Text -> Text -> Maybe (Text, Text)
findCloseHelper _ _ _ 0 acc remaining = Just (acc, remaining)
findCloseHelper tagName openStr closeStr !depth acc input
  | T.null input = Nothing
  | T.isPrefixOf closeStr input =
      if depth == 1
        then Just (acc, T.drop (T.length closeStr) input)
        else
          findCloseHelper
            tagName
            openStr
            closeStr
            (depth - 1)
            (acc <> closeStr)
            (T.drop (T.length closeStr) input)
  | T.isPrefixOf openStr input
      && case T.uncons (T.drop (T.length openStr) input) of
        Just (nc, _) -> nc == ' ' || nc == '>' || nc == '/'
        Nothing -> False =
      case T.uncons input of
        Just (ch, inputRest) ->
          findCloseHelper
            tagName
            openStr
            closeStr
            (depth + 1)
            (acc <> T.singleton ch)
            inputRest
        Nothing -> Nothing
  | otherwise =
      case T.uncons input of
        Just (ch, inputRest) ->
          findCloseHelper
            tagName
            openStr
            closeStr
            depth
            (acc <> T.singleton ch)
            inputRest
        Nothing -> Nothing

-- | Parse attribute string into key-value pairs.
parseAttributeList :: Text -> [(Text, Text)]
parseAttributeList input = go (skipWhitespace input)
  where
    go remaining = case T.uncons remaining of
      Nothing -> []
      Just (c, _)
        | not (isAttrStart c) -> []
        | otherwise ->
            let (key, afterKey) = T.span isAttrChar remaining
                afterEq = T.dropWhile (\ch -> ch == '=' || isSpace ch) afterKey
             in case T.uncons afterEq of
                  Nothing -> [(key, "")]
                  Just ('"', quotedRest) ->
                    let (val, afterVal) = T.break (== '"') quotedRest
                     in (key, val) : go (skipWhitespace (T.drop 1 afterVal))
                  Just ('\'', quotedRest) ->
                    let (val, afterVal) = T.break (== '\'') quotedRest
                     in (key, val) : go (skipWhitespace (T.drop 1 afterVal))
                  Just _ ->
                    let (val, rest) = T.break isSpace afterEq
                     in (key, val) : go (skipWhitespace rest)

-- | Skip an XML comment, returning EEmpty and remaining text.
skipComment :: Text -> Maybe (Element, Text)
skipComment input =
  case T.breakOn "-->" input of
    (_, rest)
      | T.null rest -> Nothing
      | otherwise -> Just (EEmpty, T.drop 3 rest)

-- ---------------------------------------------------------------------------
-- Internal Helpers
-- ---------------------------------------------------------------------------

skipWhitespace :: Text -> Text
skipWhitespace = T.dropWhile isSpace

skipSep :: Text -> Text
skipSep = T.dropWhile (\c -> isSpace c || c == ',')

isNumChar :: Char -> Bool
isNumChar c = isDigit c || c == '.' || c == '-' || c == '+' || c == 'e' || c == 'E'

isAttrStart :: Char -> Bool
isAttrStart c = isAlpha c || c == '_' || c == '-'

isAttrChar :: Char -> Bool
isAttrChar c = isAlpha c || isDigit c || c == '_' || c == '-' || c == ':'

addV2 :: V2 -> V2 -> V2
addV2 (V2 x1 y1) (V2 x2 y2) = V2 (x1 + x2) (y1 + y2)

groupChildren :: [Element] -> Element
groupChildren [] = EEmpty
groupChildren [single] = single
groupChildren multiple = EGroup multiple

defaultParseTextConfig :: TextConfig
defaultParseTextConfig =
  TextConfig
    { textConfigFontFamily = "sans-serif",
      textConfigFontSize = defaultFontSize,
      textConfigAnchor = AnchorStart,
      textConfigBold = False,
      textConfigItalic = False
    }

-- ---------------------------------------------------------------------------
-- Constants
-- ---------------------------------------------------------------------------

defaultDimension :: Double
defaultDimension = 300

defaultFontSize :: Double
defaultFontSize = 16

hexBase :: Int
hexBase = 16

hexLetterOffset :: Int
hexLetterOffset = 10

hexByteMax :: Double
hexByteMax = 255.0
