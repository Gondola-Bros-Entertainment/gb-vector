-- | Path manipulation operations.
--
-- Pure functions for reversing, measuring, splitting, offsetting, and
-- simplifying paths. All operations work on the 'Path' type from
-- "GBVector.Types".
module GBVector.PathOps
  ( -- * Reversal
    reversePath,

    -- * Measurement
    measurePath,

    -- * Splitting
    splitPathAt,
    subpath,

    -- * Offsetting
    offsetPath,

    -- * Simplification
    simplifyPath,
  )
where

import GBVector.Bezier (arcToCubics, cubicLength, flattenCubic)
import GBVector.Types (ArcParams (..), Path (..), Segment (..), V2 (..))

-- ---------------------------------------------------------------------------
-- Reversal
-- ---------------------------------------------------------------------------

-- | Reverse a path so it is traversed in the opposite direction.
-- A closed path remains closed; an open path remains open.
reversePath :: Path -> Path
reversePath path =
  case pathSegments path of
    [] -> path
    segs ->
      let endpoints = pathStart path : map segmentEndpoint segs
       in case reverse endpoints of
            (newStart : revRest) ->
              let reversedPairs = zip revRest (reverse segs)
                  newSegs = map (reverseSegment . snd) reversedPairs
               in path {pathStart = newStart, pathSegments = newSegs}
            [] -> path

-- ---------------------------------------------------------------------------
-- Measurement
-- ---------------------------------------------------------------------------

-- | Approximate total arc length of a path.
-- For closed paths, includes the implicit closing segment.
measurePath :: Path -> Double
measurePath path =
  let segs = pathSegments path
      openLen = foldlSegments (pathStart path) 0 segs
   in case (pathClosed path, reverse segs) of
        (True, lastSeg : _) ->
          openLen + dist (segmentEndpoint lastSeg) (pathStart path)
        _ -> openLen

-- ---------------------------------------------------------------------------
-- Splitting
-- ---------------------------------------------------------------------------

-- | Split a path at parameter @t@ in @[0, 1]@, where 0 is the start
-- and 1 is the end. Returns two open paths.
splitPathAt :: Double -> Path -> (Path, Path)
splitPathAt t path
  | t <= 0 = (emptyPathAt (pathStart path), path {pathClosed = False})
  | t >= 1 =
      let finalPt = pathEndpoint path
       in (path {pathClosed = False}, emptyPathAt finalPt)
  | otherwise =
      let segs = pathSegments path
          totalLen = measurePath path
          targetLen = t * totalLen
       in splitAtLength (pathStart path) targetLen segs

-- | Extract a sub-section of a path between parameters @t0@ and @t1@
-- in @[0, 1]@.
subpath :: Double -> Double -> Path -> Path
subpath t0 t1 path
  | t0 >= t1 = emptyPathAt (pathStart path)
  | otherwise =
      let (_, afterT0) = splitPathAt t0 path
          adjustedT1 = if t0 >= 1 then 0 else min 1.0 ((t1 - t0) / (1 - t0))
          (result, _) = splitPathAt adjustedT1 afterT0
       in result

-- ---------------------------------------------------------------------------
-- Offsetting
-- ---------------------------------------------------------------------------

-- | Offset a path by a distance. Positive offsets go outward (left of
-- the path direction), negative go inward.
--
-- This is an approximation: curves are flattened to polylines, offset,
-- then converted back to line segments. For exact parallel curves
-- use higher-order methods.
offsetPath :: Double -> Path -> Path
offsetPath amount path =
  let segs = pathSegments path
      pts = pathStart path : concatMap (flattenSegment (pathStart path)) (zip (pathStart path : map segmentEndpoint segs) segs)
      offsetPts = offsetPolyline amount pts
   in case offsetPts of
        [] -> emptyPathAt (pathStart path)
        (start : rest) ->
          Path
            { pathStart = start,
              pathSegments = map LineTo rest,
              pathClosed = pathClosed path
            }

-- ---------------------------------------------------------------------------
-- Simplification
-- ---------------------------------------------------------------------------

-- | Remove redundant points from a path within a distance tolerance.
-- Uses the Ramer-Douglas-Peucker algorithm on the path's point sequence.
simplifyPath :: Double -> Path -> Path
simplifyPath tolerance path =
  let segs = pathSegments path
      allPts = pathStart path : map segmentEndpoint segs
      simplified = rdpSimplify tolerance allPts
   in case simplified of
        [] -> emptyPathAt (pathStart path)
        [p] -> emptyPathAt p
        (start : rest) ->
          path
            { pathStart = start,
              pathSegments = map LineTo rest
            }

-- ---------------------------------------------------------------------------
-- Internal — Segment Helpers
-- ---------------------------------------------------------------------------

-- | Get the endpoint of a segment.
segmentEndpoint :: Segment -> V2
segmentEndpoint (LineTo p) = p
segmentEndpoint (CubicTo _ _ p) = p
segmentEndpoint (QuadTo _ p) = p
segmentEndpoint (ArcTo _ p) = p

-- | Approximate length of a single segment.
segmentLength :: V2 -> Segment -> Double
segmentLength from seg = case seg of
  LineTo to -> dist from to
  CubicTo c1 c2 to -> cubicLength from c1 c2 to
  QuadTo ctrl to ->
    -- Elevate to cubic for length measurement
    let c1 = lerpV (2.0 / 3.0) from ctrl
        c2 = lerpV (2.0 / 3.0) to ctrl
     in cubicLength from c1 c2 to
  ArcTo params to ->
    let cubics = arcToCubics from (arcRx params) (arcRy params) (arcRotation params) (arcLargeArc params) (arcSweep params) to
     in sumArcCubicLengths from cubics

-- | Fold over segments accumulating a value, tracking the current point.
foldlSegments :: V2 -> Double -> [Segment] -> Double
foldlSegments _ !acc [] = acc
foldlSegments from !acc (seg : rest) =
  let len = segmentLength from seg
   in foldlSegments (segmentEndpoint seg) (acc + len) rest

-- | Flatten a segment to intermediate points (not including the start).
flattenSegment :: V2 -> (V2, Segment) -> [V2]
flattenSegment _ (_, LineTo p) = [p]
flattenSegment _ (from, CubicTo c1 c2 to) = flattenCubic from c1 c2 to flattenTolerance
flattenSegment _ (from, QuadTo ctrl to) =
  let c1 = lerpV (2.0 / 3.0) from ctrl
      c2 = lerpV (2.0 / 3.0) to ctrl
   in flattenCubic from c1 c2 to flattenTolerance
flattenSegment _ (from, ArcTo params to) =
  let cubics = arcToCubics from (arcRx params) (arcRy params) (arcRotation params) (arcLargeArc params) (arcSweep params) to
   in concatMap (\(s, c1, c2, end) -> flattenCubic s c1 c2 end flattenTolerance) (arcWithStarts from cubics)

-- | Reverse a segment. Swaps control points for curves.
reverseSegment :: Segment -> Segment
reverseSegment (LineTo _) = LineTo (V2 0 0) -- Placeholder, endpoint set by caller
reverseSegment (CubicTo c1 c2 _) = CubicTo c2 c1 (V2 0 0)
reverseSegment (QuadTo c _) = QuadTo c (V2 0 0)
reverseSegment (ArcTo params _) = ArcTo params (V2 0 0)

-- ---------------------------------------------------------------------------
-- Internal — Path Helpers
-- ---------------------------------------------------------------------------

-- | Get the final endpoint of a path.
pathEndpoint :: Path -> V2
pathEndpoint path = case reverse (pathSegments path) of
  (lastSeg : _) -> segmentEndpoint lastSeg
  [] -> pathStart path

-- | Create an empty path at a given point.
emptyPathAt :: V2 -> Path
emptyPathAt p = Path p [] False

-- | Split a path at a cumulative length along its segments.
splitAtLength :: V2 -> Double -> [Segment] -> (Path, Path)
splitAtLength start _ [] = (emptyPathAt start, emptyPathAt start)
splitAtLength start targetLen (seg : rest) =
  let segLen = segmentLength start seg
   in if targetLen <= segLen
        then
          let t = if segLen > 0 then targetLen / segLen else 0
              (before, after) = splitSegmentAt start seg t
              beforePath = Path start [before] False
              afterStart = segmentEndpoint before
              afterPath = Path afterStart (after : rest) False
           in (beforePath, afterPath)
        else
          let nextPt = segmentEndpoint seg
              (Path bStart bSegs _, afterPath) = splitAtLength nextPt (targetLen - segLen) rest
           in (Path start (seg : if bStart == nextPt && null bSegs then [] else bSegs) False, afterPath)

-- | Split a single segment at parameter t.
splitSegmentAt :: V2 -> Segment -> Double -> (Segment, Segment)
splitSegmentAt from seg t = case seg of
  LineTo to ->
    let mid = lerpV t from to
     in (LineTo mid, LineTo to)
  CubicTo c1 c2 to ->
    let ((_, lc1, lc2, lEnd), (_, rc1, rc2, rEnd)) = splitCubicAtHelper from c1 c2 to t
     in (CubicTo lc1 lc2 lEnd, CubicTo rc1 rc2 rEnd)
  QuadTo ctrl to ->
    let mid = evalQuadSimple from ctrl to t
     in (LineTo mid, LineTo to)
  ArcTo params to ->
    let mid = lerpV t from to
     in (LineTo mid, ArcTo params to)

-- | De Casteljau split (inlined to avoid circular import).
splitCubicAtHelper :: V2 -> V2 -> V2 -> V2 -> Double -> ((V2, V2, V2, V2), (V2, V2, V2, V2))
splitCubicAtHelper p0 p1 p2 p3 t =
  let q0 = lerpV t p0 p1
      q1 = lerpV t p1 p2
      q2 = lerpV t p2 p3
      r0 = lerpV t q0 q1
      r1 = lerpV t q1 q2
      s0 = lerpV t r0 r1
   in ((p0, q0, r0, s0), (s0, r1, q2, p3))

-- ---------------------------------------------------------------------------
-- Internal — Offset
-- ---------------------------------------------------------------------------

-- | Offset a polyline by displacing each vertex along its normal.
offsetPolyline :: Double -> [V2] -> [V2]
offsetPolyline _ [] = []
offsetPolyline _ [p] = [p]
offsetPolyline d pts =
  let normals = computeNormals pts
   in zipWith (\(V2 px py) (V2 nx ny) -> V2 (px + d * nx) (py + d * ny)) pts normals

-- | Compute averaged normals at each vertex of a polyline.
computeNormals :: [V2] -> [V2]
computeNormals [] = []
computeNormals [_] = [V2 0 0]
computeNormals pts@(_ : ptsRest) =
  let edgeNormals = zipWith (curry edgeNormal) pts ptsRest
   in case (edgeNormals, reverse edgeNormals) of
        (firstN : normRest, lastN : _) ->
          let interiorNs = zipWith averageNormal edgeNormals normRest
           in firstN : interiorNs ++ [lastN]
        _ -> []

-- | Compute the left-facing unit normal of a directed edge.
edgeNormal :: (V2, V2) -> V2
edgeNormal (V2 x1 y1, V2 x2 y2) =
  let dx = x2 - x1
      dy = y2 - y1
      len = sqrt (dx * dx + dy * dy)
   in if len > normalEpsilon
        then V2 (-(dy / len)) (dx / len)
        else V2 0 0

-- | Average two normals and renormalize.
averageNormal :: V2 -> V2 -> V2
averageNormal (V2 nx1 ny1) (V2 nx2 ny2) =
  let ax = (nx1 + nx2) / 2
      ay = (ny1 + ny2) / 2
      len = sqrt (ax * ax + ay * ay)
   in if len > normalEpsilon
        then V2 (ax / len) (ay / len)
        else V2 0 0

-- ---------------------------------------------------------------------------
-- Internal — Ramer-Douglas-Peucker
-- ---------------------------------------------------------------------------

-- | Simplify a polyline using the Ramer-Douglas-Peucker algorithm.
rdpSimplify :: Double -> [V2] -> [V2]
rdpSimplify _ [] = []
rdpSimplify _ [p] = [p]
rdpSimplify _ [p1, p2] = [p1, p2]
rdpSimplify tol pts@(first : _) =
  case reverse pts of
    (lst : _) ->
      let (maxDist, maxIdx) = findFarthest first lst pts
       in if maxDist > tol
            then
              let (leftPart, rightPart) = splitAt maxIdx pts
                  leftSimp = rdpSimplify tol leftPart
                  rightSimp = rdpSimplify tol rightPart
               in dropLast leftSimp ++ rightSimp
            else [first, lst]
    [] -> []
  where
    dropLast [] = []
    dropLast [_] = []
    dropLast (x : xs) = x : dropLast xs

-- | Find the point farthest from the line between first and last.
findFarthest :: V2 -> V2 -> [V2] -> (Double, Int)
findFarthest start end pts =
  let distances = zip [0 ..] (map (pointLineDistance start end) pts)
   in foldl (\(!bestD, !bestI) (i, d) -> if d > bestD then (d, i) else (bestD, bestI)) (0, 0) distances

-- | Perpendicular distance from a point to a line defined by two endpoints.
pointLineDistance :: V2 -> V2 -> V2 -> Double
pointLineDistance (V2 x1 y1) (V2 x2 y2) (V2 px py) =
  let dx = x2 - x1
      dy = y2 - y1
      lenSq = dx * dx + dy * dy
   in if lenSq < normalEpsilon
        then dist (V2 x1 y1) (V2 px py)
        else abs (dy * px - dx * py + x2 * y1 - y2 * x1) / sqrt lenSq

-- ---------------------------------------------------------------------------
-- Internal — Geometry
-- ---------------------------------------------------------------------------

-- | Sum lengths of cubic segments from arc conversion.
sumArcCubicLengths :: V2 -> [(V2, V2, V2)] -> Double
sumArcCubicLengths _ [] = 0
sumArcCubicLengths start ((c1, c2, end) : rest) =
  cubicLength start c1 c2 end + sumArcCubicLengths end rest

-- | Pair each cubic segment from 'arcToCubics' with its starting point.
arcWithStarts :: V2 -> [(V2, V2, V2)] -> [(V2, V2, V2, V2)]
arcWithStarts _ [] = []
arcWithStarts start ((c1, c2, end) : rest) = (start, c1, c2, end) : arcWithStarts end rest

-- | Euclidean distance between two points.
dist :: V2 -> V2 -> Double
dist (V2 x1 y1) (V2 x2 y2) =
  let dx = x2 - x1
      dy = y2 - y1
   in sqrt (dx * dx + dy * dy)

-- | Linear interpolation between two points.
lerpV :: Double -> V2 -> V2 -> V2
lerpV t (V2 x1 y1) (V2 x2 y2) =
  V2 (x1 + t * (x2 - x1)) (y1 + t * (y2 - y1))

-- | Evaluate a quadratic bezier (simple inline).
evalQuadSimple :: V2 -> V2 -> V2 -> Double -> V2
evalQuadSimple p0 p1 p2 t =
  let q0 = lerpV t p0 p1
      q1 = lerpV t p1 p2
   in lerpV t q0 q1

-- ---------------------------------------------------------------------------
-- Internal — Constants
-- ---------------------------------------------------------------------------

-- | Tolerance for flattening curves in offset/simplify operations.
flattenTolerance :: Double
flattenTolerance = 0.5

-- | Epsilon for near-zero normal vectors.
normalEpsilon :: Double
normalEpsilon = 1.0e-10
