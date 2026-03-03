-- | Boolean operations on closed paths.
--
-- Union, intersection, difference, and symmetric difference (XOR) of
-- closed paths. Paths are flattened to polygons for boolean computation,
-- then the result is returned as a polygon 'Path'.
--
-- This is a practical polygon-clipping implementation using the
-- Sutherland-Hodgman algorithm for intersection/clipping, with
-- union and difference built on top.
--
-- __Limitations:__
--
-- * 'intersection' and 'difference' use Sutherland-Hodgman, which
--   requires the /clip/ polygon (second argument) to be __convex__.
--   Concave clip polygons will produce incorrect results.
-- * 'union' of non-overlapping polygons concatenates their vertices
--   into a single path. This renders correctly with SVG even-odd fill
--   but is not a true geometric union (no outer-boundary tracing).
-- * 'intersectEpsilon' is an absolute tolerance, not scale-aware.
--   Very large or very small coordinate spaces may need adjustment.
module GBVector.Boolean
  ( -- * Boolean Operations
    union,
    intersection,
    difference,
    xorPaths,

    -- * Polygon Utilities
    pathToPolygon,
    polygonToPath,
    polygonArea,
    pointInPolygon,
  )
where

import Data.List (foldl')
import GBVector.Bezier (arcToCubics, flattenCubic)
import GBVector.Types (ArcParams (..), Path (..), Segment (..), V2 (..))

-- ---------------------------------------------------------------------------
-- Boolean Operations
-- ---------------------------------------------------------------------------

-- | Union of two closed paths — the outline that covers both shapes.
-- Both paths are flattened to polygons.
union :: Path -> Path -> Path
union pathA pathB =
  let polyA = pathToPolygon pathA
      polyB = pathToPolygon pathB
   in if null polyA || null polyB
        then if null polyA then pathB else pathA
        else
          if polygonContainsAll polyA polyB
            then polygonToPath polyA
            else
              if polygonContainsAll polyB polyA
                then polygonToPath polyB
                else mergePolygons polyA polyB

-- | Intersection of two closed paths — the region covered by both.
-- Uses the Sutherland-Hodgman clipping algorithm.
--
-- __Note:__ @pathB@ (the clip polygon) must be convex for correct results.
intersection :: Path -> Path -> Path
intersection pathA pathB =
  let polyA = pathToPolygon pathA
      polyB = pathToPolygon pathB
      clipped = sutherlandHodgman polyA polyB
   in polygonToPath clipped

-- | Difference of two closed paths — pathA minus pathB.
-- Returns the region of A not covered by B.
--
-- __Note:__ @pathB@ (the clip polygon) must be convex for correct results.
difference :: Path -> Path -> Path
difference pathA pathB =
  let polyA = pathToPolygon pathA
      polyB = pathToPolygon pathB
      reversedB = reverse polyB
      clipped = sutherlandHodgman polyA reversedB
   in polygonToPath clipped

-- | Symmetric difference (XOR) of two closed paths — the region
-- covered by exactly one of the two shapes.
xorPaths :: Path -> Path -> Path
xorPaths pathA pathB =
  let ab = difference pathA pathB
      ba = difference pathB pathA
      polyAB = pathToPolygon ab
      polyBA = pathToPolygon ba
   in mergePolygons polyAB polyBA

-- ---------------------------------------------------------------------------
-- Polygon Utilities
-- ---------------------------------------------------------------------------

-- | Flatten a path to a list of polygon vertices.
-- Curves are approximated as line segments.
pathToPolygon :: Path -> [V2]
pathToPolygon path =
  let start = pathStart path
      segs = pathSegments path
   in start : concatMap (flattenSeg start) (zip (start : map segEndpoint segs) segs)

-- | Convert a list of polygon vertices back to a closed 'Path'.
polygonToPath :: [V2] -> Path
polygonToPath [] = Path (V2 0 0) [] False
polygonToPath [p] = Path p [] True
polygonToPath (start : rest) =
  Path
    { pathStart = start,
      pathSegments = map LineTo rest,
      pathClosed = True
    }

-- | Compute the signed area of a polygon (positive = counter-clockwise).
polygonArea :: [V2] -> Double
polygonArea [] = 0
polygonArea pts@(first : rest) =
  let pairs = zip pts (rest ++ [first])
      summed = foldl' (\acc (V2 x1 y1, V2 x2 y2) -> acc + x1 * y2 - x2 * y1) 0 pairs
   in summed / 2

-- | Test whether a point lies inside a polygon (ray casting).
pointInPolygon :: V2 -> [V2] -> Bool
pointInPolygon _ [] = False
pointInPolygon (V2 px py) poly@(first : rest) =
  let edges = zip poly (rest ++ [first])
   in odd (foldl' countCrossing 0 edges)
  where
    countCrossing :: Int -> (V2, V2) -> Int
    countCrossing !count (V2 x1 y1, V2 x2 y2)
      | (y1 <= py && y2 > py) || (y2 <= py && y1 > py) =
          let intersectX = x1 + (py - y1) / (y2 - y1) * (x2 - x1)
           in if px < intersectX then count + 1 else count
      | otherwise = count

-- ---------------------------------------------------------------------------
-- Internal — Sutherland-Hodgman Clipping
-- ---------------------------------------------------------------------------

-- | Clip a subject polygon by a convex (or general) clip polygon
-- using the Sutherland-Hodgman algorithm.
sutherlandHodgman :: [V2] -> [V2] -> [V2]
sutherlandHodgman subject [] = subject
sutherlandHodgman [] _ = []
sutherlandHodgman subject clipPoly@(first : rest) =
  let clipEdges = zip clipPoly (rest ++ [first])
   in foldl' clipByEdge subject clipEdges

-- | Clip a polygon by a single edge.
clipByEdge :: [V2] -> (V2, V2) -> [V2]
clipByEdge [] _ = []
clipByEdge poly@(first : rest) (edgeStart, edgeEnd) =
  let pairs = zip poly (rest ++ [first])
   in concatMap (clipVertex edgeStart edgeEnd) pairs

-- | Process one edge of the subject polygon against a clip edge.
clipVertex :: V2 -> V2 -> (V2, V2) -> [V2]
clipVertex edgeStart edgeEnd (current, next)
  | currentInside && nextInside = [next]
  | currentInside = [intersectEdge current next edgeStart edgeEnd]
  | nextInside = [intersectEdge current next edgeStart edgeEnd, next]
  | otherwise = []
  where
    currentInside = isInside edgeStart edgeEnd current
    nextInside = isInside edgeStart edgeEnd next

-- | Test if a point is on the inside (left) of a directed edge.
isInside :: V2 -> V2 -> V2 -> Bool
isInside (V2 ex1 ey1) (V2 ex2 ey2) (V2 px py) =
  (ex2 - ex1) * (py - ey1) - (ey2 - ey1) * (px - ex1) >= 0

-- | Find the intersection point of two line segments.
intersectEdge :: V2 -> V2 -> V2 -> V2 -> V2
intersectEdge (V2 x1 y1) (V2 x2 y2) (V2 x3 y3) (V2 x4 y4) =
  let dx12 = x1 - x2
      dy12 = y1 - y2
      dx34 = x3 - x4
      dy34 = y3 - y4
      denom = dx12 * dy34 - dy12 * dx34
   in if abs denom < intersectEpsilon
        then V2 ((x1 + x2) / 2) ((y1 + y2) / 2) -- Parallel: return midpoint
        else
          let t = ((x1 - x3) * dy34 - (y1 - y3) * dx34) / denom
           in V2 (x1 + t * (x2 - x1)) (y1 + t * (y2 - y1))

-- ---------------------------------------------------------------------------
-- Internal — Polygon Helpers
-- ---------------------------------------------------------------------------

-- | Check if all points of polygon B are inside polygon A.
polygonContainsAll :: [V2] -> [V2] -> Bool
polygonContainsAll polyA = all (`pointInPolygon` polyA)

-- | Merge two non-overlapping polygons into a single path.
-- Concatenates vertices with the larger polygon first.
-- Renders correctly with SVG even-odd fill rule.
mergePolygons :: [V2] -> [V2] -> Path
mergePolygons polyA polyB =
  let areaA = abs (polygonArea polyA)
      areaB = abs (polygonArea polyB)
      combined = if areaA >= areaB then polyA ++ polyB else polyB ++ polyA
   in polygonToPath combined

-- | Get the endpoint of a segment.
segEndpoint :: Segment -> V2
segEndpoint (LineTo p) = p
segEndpoint (CubicTo _ _ p) = p
segEndpoint (QuadTo _ p) = p
segEndpoint (ArcTo _ p) = p

-- | Flatten a segment to points (excluding the start point).
flattenSeg :: V2 -> (V2, Segment) -> [V2]
flattenSeg _ (_, LineTo p) = [p]
flattenSeg _ (from, CubicTo c1 c2 to) = flattenCubic from c1 c2 to boolFlattenTolerance
flattenSeg _ (from, QuadTo ctrl to) =
  let c1 = lerpV2 (2.0 / 3.0) from ctrl
      c2 = lerpV2 (2.0 / 3.0) to ctrl
   in flattenCubic from c1 c2 to boolFlattenTolerance
flattenSeg _ (from, ArcTo params to) =
  let cubics = arcToCubics from (arcRx params) (arcRy params) (arcRotation params) (arcLargeArc params) (arcSweep params) to
   in concatMap (\(s, c1, c2, end) -> flattenCubic s c1 c2 end boolFlattenTolerance) (withStarts from cubics)

-- | Pair each cubic segment from 'arcToCubics' with its starting point.
withStarts :: V2 -> [(V2, V2, V2)] -> [(V2, V2, V2, V2)]
withStarts _ [] = []
withStarts start ((c1, c2, end) : rest) = (start, c1, c2, end) : withStarts end rest

-- | Linear interpolation.
lerpV2 :: Double -> V2 -> V2 -> V2
lerpV2 t (V2 x1 y1) (V2 x2 y2) =
  V2 (x1 + t * (x2 - x1)) (y1 + t * (y2 - y1))

-- ---------------------------------------------------------------------------
-- Internal — Constants
-- ---------------------------------------------------------------------------

-- | Tolerance for flattening curves before boolean operations.
boolFlattenTolerance :: Double
boolFlattenTolerance = 0.5

-- | Epsilon for parallel edge detection.
-- This is an absolute tolerance — not scale-aware.
intersectEpsilon :: Double
intersectEpsilon = 1.0e-10
