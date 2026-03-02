-- | Core geometry types for SVG vector graphics.
--
-- All types use strict fields. 'V2' is the fundamental 2D point/vector.
-- 'Segment' represents a single path command (line, cubic, quad, arc).
-- 'Path' is a sequence of segments with a start point and open/closed flag.
module GBVector.Types
  ( -- * 2D Vector
    V2 (..),

    -- * Path Segments
    Segment (..),
    ArcParams (..),

    -- * Paths
    Path (..),

    -- * Enumerations
    LineCap (..),
    LineJoin (..),
    FillRule (..),
    SpreadMethod (..),

    -- * View Box
    ViewBox (..),
  )
where

-- ---------------------------------------------------------------------------
-- 2D Vector
-- ---------------------------------------------------------------------------

-- | A 2D point or vector with strict 'Double' components.
data V2 = V2 !Double !Double
  deriving (Show, Eq, Ord)

-- ---------------------------------------------------------------------------
-- Path Segments
-- ---------------------------------------------------------------------------

-- | A single segment in a path, relative to the previous endpoint.
data Segment
  = -- | Straight line to a point.
    LineTo !V2
  | -- | Cubic bezier with two control points and an endpoint.
    CubicTo !V2 !V2 !V2
  | -- | Quadratic bezier with one control point and an endpoint.
    QuadTo !V2 !V2
  | -- | Elliptical arc with parameters and an endpoint.
    ArcTo !ArcParams !V2
  deriving (Show, Eq)

-- | Parameters for an elliptical arc segment.
data ArcParams = ArcParams
  { arcRx :: !Double,
    arcRy :: !Double,
    arcRotation :: !Double,
    arcLargeArc :: !Bool,
    arcSweep :: !Bool
  }
  deriving (Show, Eq)

-- ---------------------------------------------------------------------------
-- Paths
-- ---------------------------------------------------------------------------

-- | A path: a start point, a list of segments, and whether the path is closed.
data Path = Path
  { pathStart :: !V2,
    pathSegments :: ![Segment],
    pathClosed :: !Bool
  }
  deriving (Show, Eq)

-- ---------------------------------------------------------------------------
-- Enumerations
-- ---------------------------------------------------------------------------

-- | SVG stroke line-cap style.
data LineCap = CapButt | CapRound | CapSquare
  deriving (Show, Eq, Ord, Enum, Bounded)

-- | SVG stroke line-join style.
data LineJoin = JoinMiter | JoinRound | JoinBevel
  deriving (Show, Eq, Ord, Enum, Bounded)

-- | SVG fill rule for determining interior regions.
data FillRule = FillNonZero | FillEvenOdd
  deriving (Show, Eq, Ord, Enum, Bounded)

-- | SVG gradient spread method.
data SpreadMethod = SpreadPad | SpreadReflect | SpreadRepeat
  deriving (Show, Eq, Ord, Enum, Bounded)

-- ---------------------------------------------------------------------------
-- View Box
-- ---------------------------------------------------------------------------

-- | SVG viewBox attribute: min-x, min-y, width, height.
data ViewBox = ViewBox !Double !Double !Double !Double
  deriving (Show, Eq)
