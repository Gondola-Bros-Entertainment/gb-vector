-- | Shape smart constructors.
--
-- Each function produces an 'Element' leaf node centered at the origin
-- (or sized from the origin). Compose with transforms and styles:
--
-- @translate 50 50 $ fill red $ circle 30@
module GBVector.Shape
  ( -- * Basic Shapes
    circle,
    ellipse,
    rect,
    square,
    roundedRect,
    line,

    -- * Polygons
    polygon,
    regularPolygon,
    star,

    -- * Arcs & Rings
    arc,
    ring,
  )
where

import GBVector.Element (Element (..))
import GBVector.Types (V2 (..))

-- ---------------------------------------------------------------------------
-- Basic Shapes
-- ---------------------------------------------------------------------------

-- | A circle with the given radius, centered at the origin.
circle :: Double -> Element
circle = ECircle

-- | An ellipse with the given x and y radii, centered at the origin.
ellipse :: Double -> Double -> Element
ellipse = EEllipse

-- | A rectangle with the given width and height.
-- The top-left corner is at the origin.
rect :: Double -> Double -> Element
rect = ERect

-- | A square with the given side length.
-- The top-left corner is at the origin.
square :: Double -> Element
square side = ERect side side

-- | A rounded rectangle with the given width, height, and corner radii (rx, ry).
roundedRect :: Double -> Double -> Double -> Double -> Element
roundedRect = ERoundRect

-- | A line segment between two points.
line :: V2 -> V2 -> Element
line = ELine

-- ---------------------------------------------------------------------------
-- Polygons
-- ---------------------------------------------------------------------------

-- | A polygon from a list of vertices.
polygon :: [V2] -> Element
polygon = EPolygon

-- | A regular polygon with @n@ sides and the given radius.
-- Centered at the origin, first vertex pointing up.
regularPolygon :: Int -> Double -> Element
regularPolygon n r
  | n < 3 = EPolygon []
  | otherwise = EPolygon (map vertex [0 .. n - 1])
  where
    vertex i =
      let angle = angleOffset + fromIntegral i * angleStep
       in V2 (r * cos angle) (r * sin angle)
    angleStep = twoPi / fromIntegral n
    angleOffset = -halfPi

-- | A star with the given number of points, outer radius, and inner radius.
-- Centered at the origin, first point pointing up.
star :: Int -> Double -> Double -> Element
star n outer inner
  | n < 2 = EPolygon []
  | otherwise = EPolygon (concatMap pointPair [0 .. n - 1])
  where
    pointPair i =
      let outerAngle = angleOffset + fromIntegral i * angleStep
          innerAngle = outerAngle + halfStep
       in [ V2 (outer * cos outerAngle) (outer * sin outerAngle),
            V2 (inner * cos innerAngle) (inner * sin innerAngle)
          ]
    angleStep = twoPi / fromIntegral n
    halfStep = angleStep / 2
    angleOffset = -halfPi

-- ---------------------------------------------------------------------------
-- Arcs & Rings
-- ---------------------------------------------------------------------------

-- | A circular arc centered at the origin.
-- @arc radius startAngle endAngle@ produces a polyline approximation.
-- Angles are in radians.
arc :: Double -> Double -> Double -> Element
arc r startAngle endAngle =
  EPolyline (map pointOnArc [0 .. arcSegmentCount])
  where
    pointOnArc i =
      let t = fromIntegral i / fromIntegral arcSegmentCount
          angle = startAngle + t * (endAngle - startAngle)
       in V2 (r * cos angle) (r * sin angle)

-- | A ring (annulus) centered at the origin, as two concentric circle arcs.
-- @ring outerRadius innerRadius@ produces a closed polygon.
ring :: Double -> Double -> Element
ring outer inner =
  EPolygon (outerPoints ++ reverse innerPoints)
  where
    outerPoints = map (pointOnCircle outer) [0 .. ringSegmentCount]
    innerPoints = map (pointOnCircle inner) [0 .. ringSegmentCount]
    pointOnCircle r i =
      let angle = fromIntegral i * twoPi / fromIntegral ringSegmentCount
       in V2 (r * cos angle) (r * sin angle)

-- ---------------------------------------------------------------------------
-- Constants
-- ---------------------------------------------------------------------------

twoPi :: Double
twoPi = 2 * pi

halfPi :: Double
halfPi = pi / 2

-- | Number of line segments used to approximate an arc.
arcSegmentCount :: Int
arcSegmentCount = 64

-- | Number of segments used for ring approximation.
ringSegmentCount :: Int
ringSegmentCount = 64
