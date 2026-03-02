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
  = SolidFill !Color
  | GradientFill !Gradient
  | NoFill
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
  { stopOffset :: !Double,
    stopColor :: !Color,
    stopOpacity :: !Double
  }
  deriving (Show, Eq)

-- ---------------------------------------------------------------------------
-- Stroke
-- ---------------------------------------------------------------------------

-- | Full stroke configuration with cap, join, and optional dash pattern.
data StrokeConfig = StrokeConfig
  { strokeConfigColor :: !Color,
    strokeConfigWidth :: !Double,
    strokeConfigCap :: !LineCap,
    strokeConfigJoin :: !LineJoin,
    strokeConfigDashArray :: ![Double],
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
data TextAnchor = AnchorStart | AnchorMiddle | AnchorEnd
  deriving (Show, Eq, Ord, Enum, Bounded)

-- | Configuration for rendered text.
data TextConfig = TextConfig
  { textConfigFontFamily :: !Text,
    textConfigFontSize :: !Double,
    textConfigAnchor :: !TextAnchor,
    textConfigBold :: !Bool,
    textConfigItalic :: !Bool
  }
  deriving (Show, Eq)

-- ---------------------------------------------------------------------------
-- Element
-- ---------------------------------------------------------------------------

-- | The core scene tree. Style and transforms are constructors wrapping
-- children, enabling composition via function application.
data Element
  = -- Leaf shapes
    EPath !Path
  | ECircle !Double
  | EEllipse !Double !Double
  | ERect !Double !Double
  | ERoundRect !Double !Double !Double !Double
  | ELine !V2 !V2
  | EPolyline ![V2]
  | EPolygon ![V2]
  | EText !TextConfig !Text
  | -- Grouping
    EGroup ![Element]
  | -- Style wrappers
    EFill !Fill !Element
  | EStroke !Color !Double !Element
  | EStrokeEx !StrokeConfig !Element
  | EFillRule !FillRule !Element
  | EOpacity !Double !Element
  | -- Transform wrappers
    ETranslate !Double !Double !Element
  | ERotate !Double !Element
  | ERotateAround !Double !V2 !Element
  | EScale !Double !Double !Element
  | ESkewX !Double !Element
  | ESkewY !Double !Element
  | -- Composition
    EClip !Element !Element
  | EMask !Element !Element
  | EFilter !FilterKind !Element
  | EWithId !Text !Element
  | EUse !Text
  | ERaw !Text
  | -- Accessibility
    ETitle !Text !Element
  | EDesc !Text !Element
  | EEmpty
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
  { docWidth :: !Double,
    docHeight :: !Double,
    docViewBox :: !(Maybe ViewBox),
    docElement :: !Element
  }
  deriving (Show, Eq)
