-- | Bezier curve mathematics.
--
-- De Casteljau evaluation, subdivision, bounding boxes,
-- arc-to-cubic conversion, and curve flattening.
module GBVector.Bezier
  ( -- * Evaluation
    evalCubic,
    evalQuad,

    -- * Subdivision
    splitCubicAt,
    subdivideCubic,

    -- * Bounding Box
    cubicBBox,

    -- * Arc Conversion
    arcToCubics,

    -- * Flattening
    flattenCubic,

    -- * Length
    cubicLength,
  )
where

import GBVector.Types (V2 (..))

-- ---------------------------------------------------------------------------
-- Evaluation
-- ---------------------------------------------------------------------------

-- | Evaluate a cubic bezier at parameter @t@ (De Casteljau).
evalCubic :: V2 -> V2 -> V2 -> V2 -> Double -> V2
evalCubic p0 p1 p2 p3 t =
  let q0 = lerpV2 t p0 p1
      q1 = lerpV2 t p1 p2
      q2 = lerpV2 t p2 p3
      r0 = lerpV2 t q0 q1
      r1 = lerpV2 t q1 q2
   in lerpV2 t r0 r1

-- | Evaluate a quadratic bezier at parameter @t@.
evalQuad :: V2 -> V2 -> V2 -> Double -> V2
evalQuad p0 p1 p2 t =
  let q0 = lerpV2 t p0 p1
      q1 = lerpV2 t p1 p2
   in lerpV2 t q0 q1

-- ---------------------------------------------------------------------------
-- Subdivision
-- ---------------------------------------------------------------------------

-- | Split a cubic bezier at parameter @t@, returning two cubic curves.
splitCubicAt :: V2 -> V2 -> V2 -> V2 -> Double -> ((V2, V2, V2, V2), (V2, V2, V2, V2))
splitCubicAt p0 p1 p2 p3 t =
  let q0 = lerpV2 t p0 p1
      q1 = lerpV2 t p1 p2
      q2 = lerpV2 t p2 p3
      r0 = lerpV2 t q0 q1
      r1 = lerpV2 t q1 q2
      s0 = lerpV2 t r0 r1
   in ((p0, q0, r0, s0), (s0, r1, q2, p3))

-- | Subdivide a cubic bezier at the midpoint.
subdivideCubic :: V2 -> V2 -> V2 -> V2 -> ((V2, V2, V2, V2), (V2, V2, V2, V2))
subdivideCubic p0 p1 p2 p3 = splitCubicAt p0 p1 p2 p3 0.5

-- ---------------------------------------------------------------------------
-- Bounding Box
-- ---------------------------------------------------------------------------

-- | Compute the axis-aligned bounding box of a cubic bezier.
-- Returns @(minCorner, maxCorner)@.
cubicBBox :: V2 -> V2 -> V2 -> V2 -> (V2, V2)
cubicBBox p0 p1 p2 p3 =
  let samples = map (evalCubic p0 p1 p2 p3) sampleParams
      xs = map (\(V2 x _) -> x) (p0 : p1 : p2 : p3 : samples)
      ys = map (\(V2 _ y) -> y) (p0 : p1 : p2 : p3 : samples)
   in (V2 (minimum xs) (minimum ys), V2 (maximum xs) (maximum ys))
  where
    sampleParams = map (\i -> fromIntegral i / fromIntegral bboxSamples) [1 .. bboxSamples - 1 :: Int]

-- ---------------------------------------------------------------------------
-- Arc Conversion
-- ---------------------------------------------------------------------------

-- | Convert an elliptical arc to a sequence of cubic bezier control points.
-- Each tuple is @(control1, control2, endpoint)@.
arcToCubics ::
  -- | Current point
  V2 ->
  -- | Radii (rx, ry)
  Double ->
  Double ->
  -- | X-axis rotation in degrees
  Double ->
  -- | Large arc flag
  Bool ->
  -- | Sweep flag
  Bool ->
  -- | Endpoint
  V2 ->
  [(V2, V2, V2)]
arcToCubics (V2 x1 y1) rx ry rotation largeArc sweepFlag (V2 x2 y2)
  | rx <= 0 || ry <= 0 = []
  | x1 == x2 && y1 == y2 = []
  | otherwise =
      let cosR = cos rotRad
          sinR = sin rotRad
          dx = (x1 - x2) / 2
          dy = (y1 - y2) / 2
          xPrime = cosR * dx + sinR * dy
          yPrime = -(sinR * dx) + cosR * dy
          rxSq = rx * rx
          rySq = ry * ry
          xSq = xPrime * xPrime
          ySq = yPrime * yPrime
          denom = rxSq * ySq + rySq * xSq
          num = max 0 (rxSq * rySq - denom)
          sq = sqrt (num / denom)
          signFlip = if largeArc == sweepFlag then -sq else sq
          cxPrime = signFlip * rx * yPrime / ry
          cyPrime = -(signFlip * ry * xPrime / rx)
          centerX = cosR * cxPrime - sinR * cyPrime + (x1 + x2) / 2
          centerY = sinR * cxPrime + cosR * cyPrime + (y1 + y2) / 2
          theta1 = vecAngle 1 0 ((xPrime - cxPrime) / rx) ((yPrime - cyPrime) / ry)
          dTheta0 = vecAngle ((xPrime - cxPrime) / rx) ((yPrime - cyPrime) / ry) ((-xPrime - cxPrime) / rx) ((-yPrime - cyPrime) / ry)
          dTheta
            | not sweepFlag && dTheta0 > 0 = dTheta0 - twoPi
            | sweepFlag && dTheta0 < 0 = dTheta0 + twoPi
            | otherwise = dTheta0
          segCount = max 1 (ceiling (abs dTheta / halfPi) :: Int)
          segAngle = dTheta / fromIntegral segCount
       in cubicArcSegments centerX centerY rx ry cosR sinR theta1 segAngle segCount
  where
    rotRad = rotation * pi / 180

-- ---------------------------------------------------------------------------
-- Flattening
-- ---------------------------------------------------------------------------

-- | Flatten a cubic bezier to line segments within a tolerance.
flattenCubic :: V2 -> V2 -> V2 -> V2 -> Double -> [V2]
flattenCubic p0 p1 p2 p3 tolerance
  | isFlatEnough p0 p1 p2 p3 tolerance = [p3]
  | otherwise =
      let (left, right) = subdivideCubic p0 p1 p2 p3
          (lp0, lp1, lp2, lp3) = left
          (rp0, rp1, rp2, rp3) = right
       in flattenCubic lp0 lp1 lp2 lp3 tolerance
            ++ flattenCubic rp0 rp1 rp2 rp3 tolerance

-- ---------------------------------------------------------------------------
-- Length
-- ---------------------------------------------------------------------------

-- | Approximate arc length of a cubic bezier using sampling.
cubicLength :: V2 -> V2 -> V2 -> V2 -> Double
cubicLength p0 p1 p2 p3 =
  let pts = map (evalCubic p0 p1 p2 p3) sampleParamsLength
   in sumDistances (p0 : pts)

-- ---------------------------------------------------------------------------
-- Internal
-- ---------------------------------------------------------------------------

lerpV2 :: Double -> V2 -> V2 -> V2
lerpV2 t (V2 x1 y1) (V2 x2 y2) =
  V2 (x1 + t * (x2 - x1)) (y1 + t * (y2 - y1))

isFlatEnough :: V2 -> V2 -> V2 -> V2 -> Double -> Bool
isFlatEnough (V2 x0 y0) (V2 x1 y1) (V2 x2 y2) (V2 x3 y3) tol =
  let ux = 3 * x1 - 2 * x0 - x3
      uy = 3 * y1 - 2 * y0 - y3
      vx = 3 * x2 - x0 - 2 * x3
      vy = 3 * y2 - y0 - 2 * y3
   in max (ux * ux) (vx * vx) + max (uy * uy) (vy * vy) <= 16 * tol * tol

twoPi :: Double
twoPi = 2 * pi

halfPi :: Double
halfPi = pi / 2

vecAngle :: Double -> Double -> Double -> Double -> Double
vecAngle ux uy vx vy =
  let n = sqrt (ux * ux + uy * uy) * sqrt (vx * vx + vy * vy)
      c = (ux * vx + uy * vy) / max n 1.0e-10
      s = ux * vy - uy * vx
   in atan2 s (clampNeg1to1 c)

clampNeg1to1 :: Double -> Double
clampNeg1to1 x
  | x < -1 = -1
  | x > 1 = 1
  | otherwise = x

cubicArcSegments :: Double -> Double -> Double -> Double -> Double -> Double -> Double -> Double -> Int -> [(V2, V2, V2)]
cubicArcSegments cx cy rx ry cosR sinR theta segAngle segCount =
  map mkSegment [0 .. segCount - 1]
  where
    mkSegment i =
      let t1 = theta + fromIntegral i * segAngle
          t2 = t1 + segAngle
          alpha = sin segAngle * (sqrt (4 + 3 * tan (segAngle / 2) * tan (segAngle / 2)) - 1) / 3
          p1x = cos t1
          p1y = sin t1
          p2x = cos t2
          p2y = sin t2
          cp1x = p1x - alpha * p1y
          cp1y = p1y + alpha * p1x
          cp2x = p2x + alpha * p2y
          cp2y = p2y - alpha * p2x
       in ( transformPoint cx cy rx ry cosR sinR cp1x cp1y,
            transformPoint cx cy rx ry cosR sinR cp2x cp2y,
            transformPoint cx cy rx ry cosR sinR p2x p2y
          )

transformPoint :: Double -> Double -> Double -> Double -> Double -> Double -> Double -> Double -> V2
transformPoint cx cy rx ry cosR sinR px py =
  let scaledX = rx * px
      scaledY = ry * py
   in V2
        (cosR * scaledX - sinR * scaledY + cx)
        (sinR * scaledX + cosR * scaledY + cy)

sumDistances :: [V2] -> Double
sumDistances [] = 0
sumDistances [_] = 0
sumDistances (V2 x1 y1 : rest@(V2 x2 y2 : _)) =
  let dx = x2 - x1
      dy = y2 - y1
   in sqrt (dx * dx + dy * dy) + sumDistances rest

sampleParamsLength :: [Double]
sampleParamsLength = map (\i -> fromIntegral i / fromIntegral lengthSamples) [1 .. lengthSamples :: Int]

lengthSamples :: Int
lengthSamples = 100

bboxSamples :: Int
bboxSamples = 20
