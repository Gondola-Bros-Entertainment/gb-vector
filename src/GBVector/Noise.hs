-- | Deterministic noise functions for procedural generation.
--
-- All functions are pure — no IO, no randomness. Noise is determined
-- entirely by spatial coordinates and an integer seed.
--
-- Perlin and simplex noise return values in @[-1, 1]@. Use 'fbm' for
-- layered octaves. Use 'noisePath' and 'wobblePath' to generate or
-- distort paths with noise.
module GBVector.Noise
  ( -- * 2D Noise
    perlin2D,
    simplex2D,

    -- * Fractional Brownian Motion
    fbm,

    -- * Path Generation
    noisePath,
    noiseClosedPath,

    -- * Path Distortion
    wobblePath,
    jitterPoints,

    -- * Voronoi
    voronoiCells,
    voronoiEdges,
  )
where

import Data.Bits (xor, (.&.))
import Data.List (foldl')
import GBVector.Types (Path (..), Segment (..), V2 (..))

-- ---------------------------------------------------------------------------
-- 2D Perlin Noise
-- ---------------------------------------------------------------------------

-- | 2D Perlin noise. Returns a value in approximately @[-1, 1]@.
-- Deterministic: same @(seed, x, y)@ always produces the same result.
perlin2D :: Int -> Double -> Double -> Double
perlin2D seed x y =
  let xi = floor x :: Int
      yi = floor y :: Int
      xf = x - fromIntegral xi
      yf = y - fromIntegral yi
      u = fade xf
      v = fade yf
      aa = permAt seed (permAt seed xi + yi)
      ab = permAt seed (permAt seed xi + yi + 1)
      ba = permAt seed (permAt seed (xi + 1) + yi)
      bb = permAt seed (permAt seed (xi + 1) + yi + 1)
      g00 = grad2D aa xf yf
      g10 = grad2D ba (xf - 1) yf
      g01 = grad2D ab xf (yf - 1)
      g11 = grad2D bb (xf - 1) (yf - 1)
      x0 = lerpN u g00 g10
      x1 = lerpN u g01 g11
   in lerpN v x0 x1

-- ---------------------------------------------------------------------------
-- 2D Simplex Noise
-- ---------------------------------------------------------------------------

-- | 2D simplex noise. Returns a value in approximately @[-1, 1]@.
-- Faster and less directionally biased than Perlin noise.
simplex2D :: Int -> Double -> Double -> Double
simplex2D seed x y =
  let s = (x + y) * skewFactor2D
      i = floor (x + s) :: Int
      j = floor (y + s) :: Int
      t = fromIntegral (i + j) * unskewFactor2D
      x0 = x - (fromIntegral i - t)
      y0 = y - (fromIntegral j - t)
      (i1, j1) = if x0 > y0 then (1, 0) else (0, 1)
      x1 = x0 - fromIntegral i1 + unskewFactor2D
      y1 = y0 - fromIntegral j1 + unskewFactor2D
      x2 = x0 - 1.0 + 2.0 * unskewFactor2D
      y2 = y0 - 1.0 + 2.0 * unskewFactor2D
      gi0 = permAt seed (i + permAt seed j)
      gi1 = permAt seed (i + i1 + permAt seed (j + j1))
      gi2 = permAt seed (i + 1 + permAt seed (j + 1))
      n0 = simplexContrib gi0 x0 y0
      n1 = simplexContrib gi1 x1 y1
      n2 = simplexContrib gi2 x2 y2
   in simplexScale * (n0 + n1 + n2)

-- ---------------------------------------------------------------------------
-- Fractional Brownian Motion
-- ---------------------------------------------------------------------------

-- | Layered noise via fractional Brownian motion.
--
-- @fbm noiseFn octaves lacunarity persistence x y@ sums @octaves@ layers
-- of @noiseFn@, each with increasing frequency and decreasing amplitude.
--
-- Typical values: @lacunarity = 2.0@, @persistence = 0.5@.
fbm ::
  (Double -> Double -> Double) ->
  Int ->
  Double ->
  Double ->
  Double ->
  Double ->
  Double
fbm noiseFn octaves lacunarity persistence x y =
  let step (!freq, !amp, !acc, !ampSum) _ =
        ( freq * lacunarity,
          amp * persistence,
          acc + noiseFn (x * freq) (y * freq) * amp,
          ampSum + amp
        )
      (_, _, finalTotal, finalMaxAmp) = foldl' step (1.0, 1.0, 0.0, 0.0) [1 .. octaves]
   in if finalMaxAmp > 0 then finalTotal / finalMaxAmp else 0

-- ---------------------------------------------------------------------------
-- Path Generation
-- ---------------------------------------------------------------------------

-- | Generate an open path by sampling noise at regular intervals.
--
-- @noisePath noiseFn steps xRange yAmplitude yOffset@ produces a path
-- with @steps@ segments spanning @[0, xRange]@ on X, with Y displaced
-- by noise scaled to @yAmplitude@ and shifted by @yOffset@.
noisePath ::
  (Double -> Double -> Double) ->
  Int ->
  Double ->
  Double ->
  Double ->
  Path
noisePath noiseFn steps xRange yAmplitude yOffset =
  let count = max 2 steps
      dx = xRange / fromIntegral (count - 1)
      point i =
        let px = fromIntegral i * dx
            py = yOffset + noiseFn px 0 * yAmplitude
         in V2 px py
      start = point (0 :: Int)
      segments = [LineTo (point i) | i <- [1 .. count - 1]]
   in Path start segments False

-- | Generate a closed path by sampling noise in a circle.
--
-- @noiseClosedPath noiseFn steps centerX centerY radius amplitude@ walks
-- around a circle of @radius@, displacing each point radially by noise
-- scaled to @amplitude@.
noiseClosedPath ::
  (Double -> Double -> Double) ->
  Int ->
  Double ->
  Double ->
  Double ->
  Double ->
  Path
noiseClosedPath noiseFn steps cx cy radius amplitude =
  let count = max 3 steps
      angleStep = twoPi / fromIntegral count
      point i =
        let angle = fromIntegral i * angleStep
            cosA = cos angle
            sinA = sin angle
            nx = cx + radius * cosA
            ny = cy + radius * sinA
            displacement = noiseFn nx ny * amplitude
            finalR = radius + displacement
         in V2 (cx + finalR * cosA) (cy + finalR * sinA)
      start = point (0 :: Int)
      segments = [LineTo (point i) | i <- [1 .. count - 1]]
   in Path start segments True

-- ---------------------------------------------------------------------------
-- Path Distortion
-- ---------------------------------------------------------------------------

-- | Displace each vertex of a path by noise.
--
-- @wobblePath noiseFn amplitude path@ adds noise-based displacement
-- to every point in the path.
wobblePath :: (Double -> Double -> Double) -> Double -> Path -> Path
wobblePath noiseFn amplitude path =
  let displacePoint (V2 px py) =
        let dx = noiseFn px py * amplitude
            dy = noiseFn (px + noiseOffsetX) (py + noiseOffsetY) * amplitude
         in V2 (px + dx) (py + dy)
      displaceSeg seg = case seg of
        LineTo p -> LineTo (displacePoint p)
        CubicTo c1 c2 p -> CubicTo (displacePoint c1) (displacePoint c2) (displacePoint p)
        QuadTo c p -> QuadTo (displacePoint c) (displacePoint p)
        ArcTo params p -> ArcTo params (displacePoint p)
   in path
        { pathStart = displacePoint (pathStart path),
          pathSegments = map displaceSeg (pathSegments path)
        }

-- | Randomly jitter a list of points using noise.
--
-- @jitterPoints noiseFn amplitude points@ displaces each point by
-- noise sampled at its coordinates.
jitterPoints :: (Double -> Double -> Double) -> Double -> [V2] -> [V2]
jitterPoints noiseFn amplitude = map jitter
  where
    jitter (V2 px py) =
      let dx = noiseFn px py * amplitude
          dy = noiseFn (px + noiseOffsetX) (py + noiseOffsetY) * amplitude
       in V2 (px + dx) (py + dy)

-- ---------------------------------------------------------------------------
-- Voronoi
-- ---------------------------------------------------------------------------

-- | Generate Voronoi cell center points on a grid.
--
-- @voronoiCells seed cols rows width height@ produces @cols * rows@ points,
-- each jittered within its grid cell using the hash function.
voronoiCells :: Int -> Int -> Int -> Double -> Double -> [V2]
voronoiCells seed cols rows width height =
  let cellW = width / fromIntegral (max 1 cols)
      cellH = height / fromIntegral (max 1 rows)
   in [ let cx = (fromIntegral c + 0.5) * cellW
            cy = (fromIntegral r + 0.5) * cellH
            hashVal = hashCoord seed c r
            jx = (fromIntegral (hashVal .&. jitterMask) / fromIntegral jitterMask - 0.5) * cellW * voronoiJitter
            jy = (fromIntegral ((hashVal `div` (jitterMask + 1)) .&. jitterMask) / fromIntegral jitterMask - 0.5) * cellH * voronoiJitter
         in V2 (cx + jx) (cy + jy)
      | r <- [0 .. rows - 1],
        c <- [0 .. cols - 1]
      ]

-- | Generate edges of a Voronoi diagram as line segments (pairs of points).
--
-- This is a simplified grid-based approximation: for each pair of adjacent
-- cells, the edge is the perpendicular bisector segment between the two
-- cell centers, clipped to a reasonable length.
voronoiEdges :: Int -> Int -> Int -> Double -> Double -> [(V2, V2)]
voronoiEdges seed cols rows width height =
  let cells = voronoiCells seed cols rows width height
      safeIndex idx xs = case drop idx xs of
        (v : _) -> Just v
        [] -> Nothing
      cellAt c r
        | c >= 0 && c < cols && r >= 0 && r < rows = safeIndex (r * cols + c) cells
        | otherwise = Nothing
      edgeBetween (V2 ax ay) (V2 bx by) =
        let mx = (ax + bx) / 2
            my = (ay + by) / 2
            dx = bx - ax
            dy = by - ay
            len = sqrt (dx * dx + dy * dy)
            halfEdge = len * voronoiEdgeScale
         in if len > 0
              then
                let nx = -((dy / len) * halfEdge)
                    ny = (dx / len) * halfEdge
                 in Just (V2 (mx + nx) (my + ny), V2 (mx - nx) (my - ny))
              else Nothing
      neighborPairs =
        [ (c, r, nc, nr)
        | r <- [0 .. rows - 1],
          c <- [0 .. cols - 1],
          (nc, nr) <- [(c + 1, r), (c, r + 1)]
        ]
   in concatMap
        ( \(c, r, nc, nr) -> case (cellAt c r, cellAt nc nr) of
            (Just a, Just b) -> case edgeBetween a b of
              Just edge -> [edge]
              Nothing -> []
            _ -> []
        )
        neighborPairs

-- ---------------------------------------------------------------------------
-- Internal — Permutation & Hashing
-- ---------------------------------------------------------------------------

-- | Hash-based permutation lookup. Uses bit mixing for deterministic
-- pseudo-random values without storing a permutation table.
permAt :: Int -> Int -> Int
permAt seed idx =
  let mixed = hashInt (seed + idx)
   in mixed .&. permMask

-- | Integer hash function (Thomas Wang's 32-bit mix, masked to 32 bits).
hashInt :: Int -> Int
hashInt n0 =
  let mask32 = 0xFFFFFFFF
      n1 = ((n0 + 0x7ed55d16) + (n0 * 1024)) .&. mask32
      n2 = ((n1 `xor` (n1 `div` 2048)) + 0xc761c23c) .&. mask32
      n3 = ((n2 `xor` (n2 * 65536)) + 0x165667b1) .&. mask32
      n4 = ((n3 `xor` (n3 `div` 32)) + 0xd3a2646c) .&. mask32
      n5 = ((n4 `xor` (n4 * 256)) + 0xfd7046c5) .&. mask32
      n6 = (n5 `xor` (n5 `div` 4096)) .&. mask32
   in n6

-- | Hash grid coordinates to an integer.
hashCoord :: Int -> Int -> Int -> Int
hashCoord seed cx cy = hashInt (seed + cx * coordPrimeX + cy * coordPrimeY)

-- Large primes for coordinate hashing.
coordPrimeX :: Int
coordPrimeX = 73856093

coordPrimeY :: Int
coordPrimeY = 19349663

-- ---------------------------------------------------------------------------
-- Internal — Perlin Helpers
-- ---------------------------------------------------------------------------

-- | Quintic fade curve: @6t^5 - 15t^4 + 10t^3@.
fade :: Double -> Double
fade t = t * t * t * (t * (t * 6 - 15) + 10)

-- | Linear interpolation for noise.
lerpN :: Double -> Double -> Double -> Double
lerpN t a b = a + t * (b - a)

-- | 2D gradient from a hash value.
grad2D :: Int -> Double -> Double -> Double
grad2D hash gx gy = case hash .&. 3 of
  0 -> gx + gy
  1 -> negate gx + gy
  2 -> gx - gy
  _ -> negate gx - gy

-- ---------------------------------------------------------------------------
-- Internal — Simplex Helpers
-- ---------------------------------------------------------------------------

-- | Simplex contribution from a single corner.
simplexContrib :: Int -> Double -> Double -> Double
simplexContrib gi sx sy =
  let t = simplexRadius - sx * sx - sy * sy
   in if t < 0
        then 0
        else
          let tSq = t * t
           in tSq * tSq * grad2D gi sx sy

-- ---------------------------------------------------------------------------
-- Internal — Constants
-- ---------------------------------------------------------------------------

-- | Permutation table mask (must be power of 2 minus 1).
permMask :: Int
permMask = 255

-- | Simplex skew factor for 2D: @(sqrt 3 - 1) / 2@.
skewFactor2D :: Double
skewFactor2D = 0.3660254037844386

-- | Simplex unskew factor for 2D: @(3 - sqrt 3) / 6@.
unskewFactor2D :: Double
unskewFactor2D = 0.21132486540518713

-- | Simplex corner radius squared.
simplexRadius :: Double
simplexRadius = 0.5

-- | Scale factor for simplex noise output normalization.
simplexScale :: Double
simplexScale = 70.0

-- | Noise offset for sampling a second axis (to decorrelate X/Y displacement).
noiseOffsetX :: Double
noiseOffsetX = 100.0

noiseOffsetY :: Double
noiseOffsetY = 200.0

-- | Two pi.
twoPi :: Double
twoPi = 2 * pi

-- | Jitter mask for voronoi cell randomization.
jitterMask :: Int
jitterMask = 255

-- | Voronoi cell jitter amount (0 = grid center, 1 = full cell).
voronoiJitter :: Double
voronoiJitter = 0.8

-- | Voronoi edge length scale relative to cell distance.
voronoiEdgeScale :: Double
voronoiEdgeScale = 0.4
