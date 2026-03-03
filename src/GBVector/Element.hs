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
    SolidFill !Color
  | -- | Gradient fill (linear or radial).
    GradientFill !Gradient
  | -- | No fill (transparent interior).
    NoFill
  deriving (Show, Eq)

-- ---------------------------------------------------------------------------
-- Gradient
-- ---------------------------------------------------------------------------

-- | A color gradient — linear or radial.
data Gradient
  = -- | Linear gradient from start point to end point.
    LinearGradient !V2 !V2 ![GradientStop] !SpreadMethod
  | -- | Radial gradient: center, radius, focal point, stops, spread.
    RadialGradient !V2 !Double !V2 ![GradientStop] !SpreadMethod
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
    FilterBlur !Double
  | -- | Drop shadow: dx, dy, blur, color.
    FilterDropShadow !Double !Double !Double !Color
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
    EPath !Path
  | -- | A circle with the given radius.
    ECircle !Double
  | -- | An ellipse with x and y radii.
    EEllipse !Double !Double
  | -- | A rectangle with width and height.
    ERect !Double !Double
  | -- | A rounded rectangle: width, height, corner-x radius, corner-y radius.
    ERoundRect !Double !Double !Double !Double
  | -- | A line segment between two points.
    ELine !V2 !V2
  | -- | An open polyline through the given points.
    EPolyline ![V2]
  | -- | A closed polygon through the given points.
    EPolygon ![V2]
  | -- | A text element with configuration and content.
    EText !TextConfig !Text
  | -- | A group of child elements.
    EGroup ![Element]
  | -- | Set the fill of a child element.
    EFill !Fill !Element
  | -- | Set stroke color and width on a child element.
    EStroke !Color !Double !Element
  | -- | Set full stroke configuration on a child element.
    EStrokeEx !StrokeConfig !Element
  | -- | Set the fill rule on a child element.
    EFillRule !FillRule !Element
  | -- | Set opacity on a child element.
    EOpacity !Double !Element
  | -- | Translate a child element by @(dx, dy)@.
    ETranslate !Double !Double !Element
  | -- | Rotate a child element by an angle in degrees.
    ERotate !Double !Element
  | -- | Rotate a child element around a center point.
    ERotateAround !Double !V2 !Element
  | -- | Scale a child element by @(sx, sy)@.
    EScale !Double !Double !Element
  | -- | Skew a child element along the X axis (degrees).
    ESkewX !Double !Element
  | -- | Skew a child element along the Y axis (degrees).
    ESkewY !Double !Element
  | -- | Clip a child element to a clip shape.
    EClip !Element !Element
  | -- | Mask a child element with a mask shape.
    EMask !Element !Element
  | -- | Apply a filter effect to a child element.
    EFilter !FilterKind !Element
  | -- | Assign an id to a child element (for reuse).
    EWithId !Text !Element
  | -- | Reference a previously defined element by id.
    EUse !Text
  | -- | Raw SVG text, injected verbatim.
    ERaw !Text
  | -- | Attach an accessible title to a child element.
    ETitle !Text !Element
  | -- | Attach an accessible description to a child element.
    EDesc !Text !Element
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
