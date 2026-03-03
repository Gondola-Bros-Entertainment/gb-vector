-- | Core geometry types for SVG generation.
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
  { -- | Horizontal radius of the ellipse.
    arcRx :: !Double,
    -- | Vertical radius of the ellipse.
    arcRy :: !Double,
    -- | X-axis rotation in degrees.
    arcRotation :: !Double,
    -- | Large arc flag: choose the arc spanning more than 180 degrees.
    arcLargeArc :: !Bool,
    -- | Sweep flag: choose the arc drawn in the positive-angle direction.
    arcSweep :: !Bool
  }
  deriving (Show, Eq)

-- ---------------------------------------------------------------------------
-- Paths
-- ---------------------------------------------------------------------------

-- | A path: a start point, a list of segments, and whether the path is closed.
data Path = Path
  { -- | Starting point of the path.
    pathStart :: !V2,
    -- | Ordered list of segments following the start point.
    pathSegments :: ![Segment],
    -- | Whether the path is closed (connects back to start).
    pathClosed :: !Bool
  }
  deriving (Show, Eq)

-- ---------------------------------------------------------------------------
-- Enumerations
-- ---------------------------------------------------------------------------

-- | SVG stroke line-cap style.
data LineCap
  = -- | Flat edge at the endpoint (default).
    CapButt
  | -- | Semicircular cap extending past the endpoint.
    CapRound
  | -- | Square cap extending past the endpoint.
    CapSquare
  deriving (Show, Eq, Ord, Enum, Bounded)

-- | SVG stroke line-join style.
data LineJoin
  = -- | Sharp corner (default).
    JoinMiter
  | -- | Rounded corner.
    JoinRound
  | -- | Bevelled (flattened) corner.
    JoinBevel
  deriving (Show, Eq, Ord, Enum, Bounded)

-- | SVG fill rule for determining interior regions.
data FillRule
  = -- | Non-zero winding rule (default).
    FillNonZero
  | -- | Even-odd rule — alternating inside/outside.
    FillEvenOdd
  deriving (Show, Eq, Ord, Enum, Bounded)

-- | SVG gradient spread method.
data SpreadMethod
  = -- | Extend the final color to fill the remaining area (default).
    SpreadPad
  | -- | Reflect the gradient pattern.
    SpreadReflect
  | -- | Repeat the gradient pattern.
    SpreadRepeat
  deriving (Show, Eq, Ord, Enum, Bounded)

-- ---------------------------------------------------------------------------
-- View Box
-- ---------------------------------------------------------------------------

-- | SVG viewBox attribute: min-x, min-y, width, height.
data ViewBox = ViewBox !Double !Double !Double !Double
  deriving (Show, Eq)
