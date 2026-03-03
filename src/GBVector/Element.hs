-- | The core scene tree type and its supporting types.
--
-- 'Element' is a recursive sum type. Style and transforms are constructors
-- that wrap children, enabling composition via function application:
--
-- @translate 50 50 $ fill gold $ circle 30@
module GBVector.Element
  ( -- * Scene Tree
    Element (..),

    -- * Fill
    Fill (..),

    -- * Gradient
    Gradient (..),
    GradientStop (..),

    -- * Stroke
    StrokeConfig (..),

    -- * Filters
    FilterKind (..),

    -- * Text
    TextConfig (..),
    TextAnchor (..),

    -- * Document
    Document (..),
  )
where

import Data.Text (Text)
import GBVector.Color (Color)
import GBVector.Types
  ( FillRule,
    LineCap,
    LineJoin,
    Path,
    SpreadMethod,
    V2,
    ViewBox,
  )

-- ---------------------------------------------------------------------------
-- Fill
-- ---------------------------------------------------------------------------

-- | How an element's interior is painted.
data Fill
  = -- | Flat color fill.
    SolidFill
      -- | Fill color.
      !Color
  | -- | Gradient fill (linear or radial).
    GradientFill
      -- | Gradient definition.
      !Gradient
  | -- | No fill (transparent interior).
    NoFill
  deriving (Show, Eq)

-- ---------------------------------------------------------------------------
-- Gradient
-- ---------------------------------------------------------------------------

-- | A color gradient — linear or radial.
data Gradient
  = -- | Linear gradient from start point to end point.
    LinearGradient
      -- | Start point.
      !V2
      -- | End point.
      !V2
      -- | Color stops.
      ![GradientStop]
      -- | Spread method beyond the gradient bounds.
      !SpreadMethod
  | -- | Radial gradient: center, radius, focal point, stops, spread.
    RadialGradient
      -- | Center point.
      !V2
      -- | Radius.
      !Double
      -- | Focal point.
      !V2
      -- | Color stops.
      ![GradientStop]
      -- | Spread method beyond the gradient bounds.
      !SpreadMethod
  deriving (Show, Eq)

-- | A single color stop in a gradient.
data GradientStop = GradientStop
  { -- | Position along the gradient in @[0, 1]@.
    stopOffset :: !Double,
    -- | Color at this stop.
    stopColor :: !Color,
    -- | Opacity at this stop in @[0, 1]@.
    stopOpacity :: !Double
  }
  deriving (Show, Eq)

-- ---------------------------------------------------------------------------
-- Stroke
-- ---------------------------------------------------------------------------

-- | Full stroke configuration with cap, join, and optional dash pattern.
data StrokeConfig = StrokeConfig
  { -- | Stroke color.
    strokeConfigColor :: !Color,
    -- | Stroke width in user units.
    strokeConfigWidth :: !Double,
    -- | Line cap style (butt, round, or square).
    strokeConfigCap :: !LineCap,
    -- | Line join style (miter, round, or bevel).
    strokeConfigJoin :: !LineJoin,
    -- | Dash pattern lengths (empty for solid stroke).
    strokeConfigDashArray :: ![Double],
    -- | Offset into the dash pattern.
    strokeConfigDashOffset :: !Double
  }
  deriving (Show, Eq)

-- ---------------------------------------------------------------------------
-- Filters
-- ---------------------------------------------------------------------------

-- | SVG filter effects.
data FilterKind
  = -- | Gaussian blur with standard deviation.
    FilterBlur
      -- | Standard deviation (blur radius).
      !Double
  | -- | Drop shadow: dx, dy, blur, color.
    FilterDropShadow
      -- | Horizontal offset.
      !Double
      -- | Vertical offset.
      !Double
      -- | Blur standard deviation.
      !Double
      -- | Shadow color.
      !Color
  deriving (Show, Eq)

-- ---------------------------------------------------------------------------
-- Text
-- ---------------------------------------------------------------------------

-- | Text anchor position.
data TextAnchor
  = -- | Align to the start (left for LTR text).
    AnchorStart
  | -- | Align to the center.
    AnchorMiddle
  | -- | Align to the end (right for LTR text).
    AnchorEnd
  deriving (Show, Eq, Ord, Enum, Bounded)

-- | Configuration for rendered text.
data TextConfig = TextConfig
  { -- | Font family name (e.g. @\"sans-serif\"@).
    textConfigFontFamily :: !Text,
    -- | Font size in user units.
    textConfigFontSize :: !Double,
    -- | Horizontal text anchor alignment.
    textConfigAnchor :: !TextAnchor,
    -- | Whether to render in bold weight.
    textConfigBold :: !Bool,
    -- | Whether to render in italic style.
    textConfigItalic :: !Bool
  }
  deriving (Show, Eq)

-- ---------------------------------------------------------------------------
-- Element
-- ---------------------------------------------------------------------------

-- | The core scene tree. Style and transforms are constructors wrapping
-- children, enabling composition via function application.
data Element
  = -- | An SVG path shape.
    EPath
      -- | Path geometry.
      !Path
  | -- | A circle with the given radius.
    ECircle
      -- | Radius.
      !Double
  | -- | An ellipse with x and y radii.
    EEllipse
      -- | X radius.
      !Double
      -- | Y radius.
      !Double
  | -- | A rectangle with width and height.
    ERect
      -- | Width.
      !Double
      -- | Height.
      !Double
  | -- | A rounded rectangle: width, height, corner-x radius, corner-y radius.
    ERoundRect
      -- | Width.
      !Double
      -- | Height.
      !Double
      -- | Corner X radius.
      !Double
      -- | Corner Y radius.
      !Double
  | -- | A line segment between two points.
    ELine
      -- | Start point.
      !V2
      -- | End point.
      !V2
  | -- | An open polyline through the given points.
    EPolyline
      -- | Vertices.
      ![V2]
  | -- | A closed polygon through the given points.
    EPolygon
      -- | Vertices.
      ![V2]
  | -- | A text element with configuration and content.
    EText
      -- | Text configuration.
      !TextConfig
      -- | Text content.
      !Text
  | -- | A group of child elements.
    EGroup
      -- | Children.
      ![Element]
  | -- | Set the fill of a child element.
    EFill
      -- | Fill style.
      !Fill
      -- | Child element.
      !Element
  | -- | Set stroke color and width on a child element.
    EStroke
      -- | Stroke color.
      !Color
      -- | Stroke width.
      !Double
      -- | Child element.
      !Element
  | -- | Set full stroke configuration on a child element.
    EStrokeEx
      -- | Stroke configuration.
      !StrokeConfig
      -- | Child element.
      !Element
  | -- | Set the fill rule on a child element.
    EFillRule
      -- | Fill rule.
      !FillRule
      -- | Child element.
      !Element
  | -- | Set opacity on a child element.
    EOpacity
      -- | Opacity in @[0, 1]@.
      !Double
      -- | Child element.
      !Element
  | -- | Translate a child element by @(dx, dy)@.
    ETranslate
      -- | X offset.
      !Double
      -- | Y offset.
      !Double
      -- | Child element.
      !Element
  | -- | Rotate a child element by an angle in degrees.
    ERotate
      -- | Angle in degrees.
      !Double
      -- | Child element.
      !Element
  | -- | Rotate a child element around a center point.
    ERotateAround
      -- | Angle in degrees.
      !Double
      -- | Center of rotation.
      !V2
      -- | Child element.
      !Element
  | -- | Scale a child element by @(sx, sy)@.
    EScale
      -- | X scale factor.
      !Double
      -- | Y scale factor.
      !Double
      -- | Child element.
      !Element
  | -- | Skew a child element along the X axis (degrees).
    ESkewX
      -- | Skew angle in degrees.
      !Double
      -- | Child element.
      !Element
  | -- | Skew a child element along the Y axis (degrees).
    ESkewY
      -- | Skew angle in degrees.
      !Double
      -- | Child element.
      !Element
  | -- | Clip a child element to a clip shape.
    EClip
      -- | Clip shape.
      !Element
      -- | Child element.
      !Element
  | -- | Mask a child element with a mask shape.
    EMask
      -- | Mask shape.
      !Element
      -- | Child element.
      !Element
  | -- | Apply a filter effect to a child element.
    EFilter
      -- | Filter effect.
      !FilterKind
      -- | Child element.
      !Element
  | -- | Assign an id to a child element (for reuse).
    EWithId
      -- | Element id.
      !Text
      -- | Child element.
      !Element
  | -- | Reference a previously defined element by id.
    EUse
      -- | Referenced id.
      !Text
  | -- | Raw SVG text, injected verbatim.
    ERaw
      -- | Raw SVG markup.
      !Text
  | -- | Attach an accessible title to a child element.
    ETitle
      -- | Title text.
      !Text
      -- | Child element.
      !Element
  | -- | Attach an accessible description to a child element.
    EDesc
      -- | Description text.
      !Text
      -- | Child element.
      !Element
  | -- | The empty element (identity for 'Monoid').
    EEmpty
  deriving (Show, Eq)

-- | Combine two elements into a group.
instance Semigroup Element where
  EEmpty <> b = b
  a <> EEmpty = a
  EGroup as <> EGroup bs = EGroup (as ++ bs)
  EGroup as <> b = EGroup (as ++ [b])
  a <> EGroup bs = EGroup (a : bs)
  a <> b = EGroup [a, b]

-- | 'mempty' is 'EEmpty'.
instance Monoid Element where
  mempty = EEmpty

-- ---------------------------------------------------------------------------
-- Document
-- ---------------------------------------------------------------------------

-- | A complete SVG document with dimensions and a root element.
data Document = Document
  { -- | Document width in user units.
    docWidth :: !Double,
    -- | Document height in user units.
    docHeight :: !Double,
    -- | Optional SVG viewBox for coordinate mapping.
    docViewBox :: !(Maybe ViewBox),
    -- | Root element of the document tree.
    docElement :: !Element
  }
  deriving (Show, Eq)
