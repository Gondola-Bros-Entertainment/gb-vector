{-# LANGUAGE OverloadedStrings #-}

-- | SVG rendering — pure serialization of the 'Element' tree to SVG 'Text'.
--
-- The only IO in this library lives here: 'writeSvg'.
module GBVector.SVG
  ( -- * Rendering
    render,
    renderCompact,
    renderElement,

    -- * File Output
    writeSvg,
  )
where

import Data.List (foldl')
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import GBVector.Color (toHex)
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

-- ---------------------------------------------------------------------------
-- Rendering
-- ---------------------------------------------------------------------------

-- | Render a t'Document' to SVG text with newlines.
render :: Document -> Text
render doc =
  let (defs, body) = collectDefs (docElement doc)
      defsBlock = renderDefs defs
   in svgHeader doc <> defsBlock <> body <> svgFooter

-- | Render a t'Document' to compact SVG (no extra whitespace).
renderCompact :: Document -> Text
renderCompact = render

-- | Render a single 'Element' to an SVG fragment (no surrounding @\<svg\>@ tag).
renderElement :: Element -> Text
renderElement el =
  let (defs, body) = collectDefs el
   in renderDefs defs <> body

-- | Write a t'Document' to a file.
writeSvg :: FilePath -> Document -> IO ()
writeSvg path doc = TIO.writeFile path (render doc)

-- ---------------------------------------------------------------------------
-- SVG Document Structure
-- ---------------------------------------------------------------------------

svgHeader :: Document -> Text
svgHeader doc =
  "<svg xmlns=\"http://www.w3.org/2000/svg\""
    <> " width=\""
    <> showDouble (docWidth doc)
    <> "\""
    <> " height=\""
    <> showDouble (docHeight doc)
    <> "\""
    <> renderViewBox (docViewBox doc)
    <> ">"

renderViewBox :: Maybe ViewBox -> Text
renderViewBox Nothing = T.empty
renderViewBox (Just (ViewBox minX minY w h)) =
  " viewBox=\""
    <> showDouble minX
    <> " "
    <> showDouble minY
    <> " "
    <> showDouble w
    <> " "
    <> showDouble h
    <> "\""

svgFooter :: Text
svgFooter = "</svg>"

-- ---------------------------------------------------------------------------
-- Defs Collection
-- ---------------------------------------------------------------------------

-- | State accumulated during tree traversal for defs.
data DefsState = DefsState
  { dsGradients :: ![GradientDef],
    dsFilters :: ![FilterDef],
    dsClipPaths :: ![ClipDef],
    dsMasks :: ![MaskDef],
    dsCounter :: !Int
  }

data GradientDef = GradientDef !Text !Gradient

data FilterDef = FilterDef !Text !FilterKind

data ClipDef = ClipDef !Text !Element

data MaskDef = MaskDef !Text !Element

emptyDefs :: DefsState
emptyDefs = DefsState [] [] [] [] 0

freshId :: Text -> DefsState -> (Text, DefsState)
freshId prefix ds =
  let n = dsCounter ds
      genId = prefix <> T.pack (show n)
   in (genId, ds {dsCounter = n + 1})

-- | Walk the element tree, extracting defs and producing rendered body text.
collectDefs :: Element -> ([Text], Text)
collectDefs el =
  let (body, ds) = renderEl el emptyDefs
      allDefs =
        map renderGradientDef (reverse (dsGradients ds))
          ++ map renderFilterDef (reverse (dsFilters ds))
          ++ map renderClipDef (reverse (dsClipPaths ds))
          ++ map renderMaskDef (reverse (dsMasks ds))
   in (allDefs, body)

-- ---------------------------------------------------------------------------
-- Element Rendering
-- ---------------------------------------------------------------------------

renderEl :: Element -> DefsState -> (Text, DefsState)
renderEl EEmpty ds = (T.empty, ds)
renderEl (ECircle r) ds =
  (tag "circle" [attr "cx" "0", attr "cy" "0", attr "r" (showDouble r)] Nothing, ds)
renderEl (EEllipse rx ry) ds =
  (tag "ellipse" [attr "cx" "0", attr "cy" "0", attr "rx" (showDouble rx), attr "ry" (showDouble ry)] Nothing, ds)
renderEl (ERect w h) ds =
  (tag "rect" [attr "width" (showDouble w), attr "height" (showDouble h)] Nothing, ds)
renderEl (ERoundRect w h rx ry) ds =
  ( tag
      "rect"
      [ attr "width" (showDouble w),
        attr "height" (showDouble h),
        attr "rx" (showDouble rx),
        attr "ry" (showDouble ry)
      ]
      Nothing,
    ds
  )
renderEl (ELine (V2 x1 y1) (V2 x2 y2)) ds =
  ( tag
      "line"
      [ attr "x1" (showDouble x1),
        attr "y1" (showDouble y1),
        attr "x2" (showDouble x2),
        attr "y2" (showDouble y2)
      ]
      Nothing,
    ds
  )
renderEl (EPolyline pts) ds =
  (tag "polyline" [attr "points" (renderPoints pts), attr "fill" "none"] Nothing, ds)
renderEl (EPolygon pts) ds =
  (tag "polygon" [attr "points" (renderPoints pts)] Nothing, ds)
renderEl (EPath path) ds =
  (tag "path" [attr "d" (renderPathData path)] Nothing, ds)
renderEl (EText config content) ds =
  (tag "text" (textAttrs config) (Just (escapeXml content)), ds)
renderEl (EGroup children) ds =
  let (childTexts, finalDs) = renderChildren children ds
   in (tagWrap "g" [] childTexts, finalDs)
renderEl (EFill fillVal child) ds =
  case fillVal of
    SolidFill c ->
      let (childText, childDs) = renderEl child ds
       in (wrapG [attr "fill" (T.pack (toHex c))] childText, childDs)
    GradientFill grad ->
      let (gradId, ds1) = freshId "grad" ds
          ds2 = ds1 {dsGradients = GradientDef gradId grad : dsGradients ds1}
          (childText, ds3) = renderEl child ds2
       in (wrapG [attr "fill" ("url(#" <> gradId <> ")")] childText, ds3)
    NoFill ->
      let (childText, childDs) = renderEl child ds
       in (wrapG [attr "fill" "none"] childText, childDs)
renderEl (EStroke c w child) ds =
  let (childText, childDs) = renderEl child ds
   in ( wrapG
          [ attr "stroke" (T.pack (toHex c)),
            attr "stroke-width" (showDouble w)
          ]
          childText,
        childDs
      )
renderEl (EStrokeEx config child) ds =
  let (childText, childDs) = renderEl child ds
   in (wrapG (strokeConfigAttrs config) childText, childDs)
renderEl (EFillRule rule child) ds =
  let (childText, childDs) = renderEl child ds
   in (wrapG [attr "fill-rule" (renderFillRule rule)] childText, childDs)
renderEl (EOpacity a child) ds =
  let (childText, childDs) = renderEl child ds
   in (wrapG [attr "opacity" (showDouble a)] childText, childDs)
renderEl (ETranslate dx dy child) ds =
  let (childText, childDs) = renderEl child ds
   in ( wrapG
          [attr "transform" ("translate(" <> showDouble dx <> "," <> showDouble dy <> ")")]
          childText,
        childDs
      )
renderEl (ERotate deg child) ds =
  let (childText, childDs) = renderEl child ds
   in ( wrapG
          [attr "transform" ("rotate(" <> showDouble deg <> ")")]
          childText,
        childDs
      )
renderEl (ERotateAround deg (V2 cx cy) child) ds =
  let (childText, childDs) = renderEl child ds
   in ( wrapG
          [ attr "transform" $
              "rotate("
                <> showDouble deg
                <> ","
                <> showDouble cx
                <> ","
                <> showDouble cy
                <> ")"
          ]
          childText,
        childDs
      )
renderEl (EScale sx sy child) ds =
  let (childText, childDs) = renderEl child ds
   in ( wrapG
          [attr "transform" ("scale(" <> showDouble sx <> "," <> showDouble sy <> ")")]
          childText,
        childDs
      )
renderEl (ESkewX deg child) ds =
  let (childText, childDs) = renderEl child ds
   in ( wrapG
          [attr "transform" ("skewX(" <> showDouble deg <> ")")]
          childText,
        childDs
      )
renderEl (ESkewY deg child) ds =
  let (childText, childDs) = renderEl child ds
   in ( wrapG
          [attr "transform" ("skewY(" <> showDouble deg <> ")")]
          childText,
        childDs
      )
renderEl (EClip clipEl child) ds =
  let (clipId, ds1) = freshId "clip" ds
      ds2 = ds1 {dsClipPaths = ClipDef clipId clipEl : dsClipPaths ds1}
      (childText, ds3) = renderEl child ds2
   in (wrapG [attr "clip-path" ("url(#" <> clipId <> ")")] childText, ds3)
renderEl (EMask maskEl child) ds =
  let (maskId, ds1) = freshId "mask" ds
      ds2 = ds1 {dsMasks = MaskDef maskId maskEl : dsMasks ds1}
      (childText, ds3) = renderEl child ds2
   in (wrapG [attr "mask" ("url(#" <> maskId <> ")")] childText, ds3)
renderEl (EFilter fk child) ds =
  let (filterId, ds1) = freshId "filter" ds
      ds2 = ds1 {dsFilters = FilterDef filterId fk : dsFilters ds1}
      (childText, ds3) = renderEl child ds2
   in (wrapG [attr "filter" ("url(#" <> filterId <> ")")] childText, ds3)
renderEl (EWithId elId child) ds =
  let (childText, childDs) = renderEl child ds
   in (wrapG [attr "id" elId] childText, childDs)
renderEl (EUse refId) ds =
  (tag "use" [attr "href" ("#" <> refId)] Nothing, ds)
renderEl (ERaw t) ds = (t, ds)
renderEl (ETitle titleText child) ds =
  let (childText, childDs) = renderEl child ds
      titleTag = "<title>" <> escapeXml titleText <> "</title>"
   in (wrapG [] (titleTag <> childText), childDs)
renderEl (EDesc descText child) ds =
  let (childText, childDs) = renderEl child ds
      descTag = "<desc>" <> escapeXml descText <> "</desc>"
   in (wrapG [] (descTag <> childText), childDs)

-- ---------------------------------------------------------------------------
-- Children Rendering
-- ---------------------------------------------------------------------------

renderChildren :: [Element] -> DefsState -> (Text, DefsState)
renderChildren els ds =
  let step (accText, accDs) el =
        let (elText, newDs) = renderEl el accDs
         in (accText <> elText, newDs)
   in foldl' step (T.empty, ds) els

-- ---------------------------------------------------------------------------
-- Defs Rendering
-- ---------------------------------------------------------------------------

renderDefs :: [Text] -> Text
renderDefs [] = T.empty
renderDefs items = "<defs>" <> T.concat items <> "</defs>"

renderGradientDef :: GradientDef -> Text
renderGradientDef (GradientDef gid (LinearGradient (V2 x1 y1) (V2 x2 y2) stops spread)) =
  "<linearGradient"
    <> attr "id" gid
    <> attr "x1" (showDouble x1)
    <> attr "y1" (showDouble y1)
    <> attr "x2" (showDouble x2)
    <> attr "y2" (showDouble y2)
    <> attr "spreadMethod" (renderSpreadMethod spread)
    <> ">"
    <> T.concat (map renderStop stops)
    <> "</linearGradient>"
renderGradientDef (GradientDef gid (RadialGradient (V2 cx cy) r (V2 fx fy) stops spread)) =
  "<radialGradient"
    <> attr "id" gid
    <> attr "cx" (showDouble cx)
    <> attr "cy" (showDouble cy)
    <> attr "r" (showDouble r)
    <> attr "fx" (showDouble fx)
    <> attr "fy" (showDouble fy)
    <> attr "spreadMethod" (renderSpreadMethod spread)
    <> ">"
    <> T.concat (map renderStop stops)
    <> "</radialGradient>"

renderStop :: GradientStop -> Text
renderStop (GradientStop offset c stopOp) =
  "<stop"
    <> attr "offset" (showDouble offset)
    <> attr "stop-color" (T.pack (toHex c))
    <> (if stopOp < 1.0 then attr "stop-opacity" (showDouble stopOp) else T.empty)
    <> "/>"

renderFilterDef :: FilterDef -> Text
renderFilterDef (FilterDef fid (FilterBlur stdDev)) =
  "<filter"
    <> attr "id" fid
    <> ">"
    <> "<feGaussianBlur"
    <> attr "stdDeviation" (showDouble stdDev)
    <> "/>"
    <> "</filter>"
renderFilterDef (FilterDef fid (FilterDropShadow dx dy blurAmt shadowColor)) =
  "<filter"
    <> attr "id" fid
    <> ">"
    <> "<feDropShadow"
    <> attr "dx" (showDouble dx)
    <> attr "dy" (showDouble dy)
    <> attr "stdDeviation" (showDouble blurAmt)
    <> attr "flood-color" (T.pack (toHex shadowColor))
    <> "/>"
    <> "</filter>"

renderClipDef :: ClipDef -> Text
renderClipDef (ClipDef cid el) =
  let (body, _) = renderEl el emptyDefs
   in "<clipPath" <> attr "id" cid <> ">" <> body <> "</clipPath>"

renderMaskDef :: MaskDef -> Text
renderMaskDef (MaskDef mid el) =
  let (body, _) = renderEl el emptyDefs
   in "<mask" <> attr "id" mid <> ">" <> body <> "</mask>"

-- ---------------------------------------------------------------------------
-- Path Data
-- ---------------------------------------------------------------------------

renderPathData :: Path -> Text
renderPathData (Path (V2 sx sy) segs closed) =
  "M"
    <> showDouble sx
    <> " "
    <> showDouble sy
    <> T.concat (map renderSegment segs)
    <> (if closed then "Z" else T.empty)

renderSegment :: Segment -> Text
renderSegment (LineTo (V2 x y)) =
  "L" <> showDouble x <> " " <> showDouble y
renderSegment (CubicTo (V2 cx1 cy1) (V2 cx2 cy2) (V2 x y)) =
  "C"
    <> showDouble cx1
    <> " "
    <> showDouble cy1
    <> " "
    <> showDouble cx2
    <> " "
    <> showDouble cy2
    <> " "
    <> showDouble x
    <> " "
    <> showDouble y
renderSegment (QuadTo (V2 cx cy) (V2 x y)) =
  "Q"
    <> showDouble cx
    <> " "
    <> showDouble cy
    <> " "
    <> showDouble x
    <> " "
    <> showDouble y
renderSegment (ArcTo (ArcParams rx ry rot la sw) (V2 x y)) =
  "A"
    <> showDouble rx
    <> " "
    <> showDouble ry
    <> " "
    <> showDouble rot
    <> " "
    <> (if la then "1" else "0")
    <> " "
    <> (if sw then "1" else "0")
    <> " "
    <> showDouble x
    <> " "
    <> showDouble y

-- ---------------------------------------------------------------------------
-- Attribute Helpers
-- ---------------------------------------------------------------------------

attr :: Text -> Text -> Text
attr name val = " " <> name <> "=\"" <> val <> "\""

tag :: Text -> [Text] -> Maybe Text -> Text
tag name attrs Nothing =
  "<" <> name <> T.concat attrs <> "/>"
tag name attrs (Just content) =
  "<" <> name <> T.concat attrs <> ">" <> content <> "</" <> name <> ">"

tagWrap :: Text -> [Text] -> Text -> Text
tagWrap name attrs content =
  "<" <> name <> T.concat attrs <> ">" <> content <> "</" <> name <> ">"

wrapG :: [Text] -> Text -> Text
wrapG = tagWrap "g"

-- ---------------------------------------------------------------------------
-- Text Rendering
-- ---------------------------------------------------------------------------

textAttrs :: TextConfig -> [Text]
textAttrs config =
  [ attr "font-family" (textConfigFontFamily config),
    attr "font-size" (showDouble (textConfigFontSize config)),
    attr "text-anchor" (renderTextAnchor (textConfigAnchor config))
  ]
    ++ [attr "font-weight" "bold" | textConfigBold config]
    ++ [attr "font-style" "italic" | textConfigItalic config]

renderTextAnchor :: TextAnchor -> Text
renderTextAnchor AnchorStart = "start"
renderTextAnchor AnchorMiddle = "middle"
renderTextAnchor AnchorEnd = "end"

-- ---------------------------------------------------------------------------
-- Stroke Config
-- ---------------------------------------------------------------------------

strokeConfigAttrs :: StrokeConfig -> [Text]
strokeConfigAttrs config =
  [ attr "stroke" (T.pack (toHex (strokeConfigColor config))),
    attr "stroke-width" (showDouble (strokeConfigWidth config)),
    attr "stroke-linecap" (renderLineCap (strokeConfigCap config)),
    attr "stroke-linejoin" (renderLineJoin (strokeConfigJoin config))
  ]
    ++ dashAttrs
  where
    dashAttrs = case strokeConfigDashArray config of
      [] -> []
      dashes ->
        [ attr "stroke-dasharray" (T.intercalate "," (map showDouble dashes)),
          attr "stroke-dashoffset" (showDouble (strokeConfigDashOffset config))
        ]

renderLineCap :: LineCap -> Text
renderLineCap CapButt = "butt"
renderLineCap CapRound = "round"
renderLineCap CapSquare = "square"

renderLineJoin :: LineJoin -> Text
renderLineJoin JoinMiter = "miter"
renderLineJoin JoinRound = "round"
renderLineJoin JoinBevel = "bevel"

-- ---------------------------------------------------------------------------
-- Enum Rendering
-- ---------------------------------------------------------------------------

renderFillRule :: FillRule -> Text
renderFillRule FillNonZero = "nonzero"
renderFillRule FillEvenOdd = "evenodd"

renderSpreadMethod :: SpreadMethod -> Text
renderSpreadMethod SpreadPad = "pad"
renderSpreadMethod SpreadReflect = "reflect"
renderSpreadMethod SpreadRepeat = "repeat"

-- ---------------------------------------------------------------------------
-- Points & Numbers
-- ---------------------------------------------------------------------------

renderPoints :: [V2] -> Text
renderPoints pts = T.intercalate " " (map renderPoint pts)

renderPoint :: V2 -> Text
renderPoint (V2 x y) = showDouble x <> "," <> showDouble y

-- | Render a 'Double' to 'Text', stripping trailing zeros.
showDouble :: Double -> Text
showDouble d
  | d == fromInteger rounded = T.pack (show rounded)
  | otherwise = T.pack (stripTrailingZeros (show d))
  where
    rounded = round d :: Integer

-- ---------------------------------------------------------------------------
-- XML Escaping
-- ---------------------------------------------------------------------------

escapeXml :: Text -> Text
escapeXml = T.concatMap escapeChar
  where
    escapeChar '<' = "&lt;"
    escapeChar '>' = "&gt;"
    escapeChar '&' = "&amp;"
    escapeChar '"' = "&quot;"
    escapeChar c = T.singleton c

-- ---------------------------------------------------------------------------
-- Internal
-- ---------------------------------------------------------------------------

stripTrailingZeros :: String -> String
stripTrailingZeros s =
  case break (== '.') s of
    (_, "") -> s
    (whole, frac) ->
      let stripped = dropWhileEnd (== '0') frac
       in if stripped == "."
            then whole
            else whole ++ stripped

dropWhileEnd :: (Char -> Bool) -> String -> String
dropWhileEnd p = reverse . dropWhile p . reverse
