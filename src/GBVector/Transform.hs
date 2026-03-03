-- | Transform smart constructors and affine matrix math.
--
-- Each function wraps an 'Element' in a transform constructor.
-- Compose naturally:
--
-- @translate 100 100 $ rotate 45 $ scale 2 $ circle 30@
--
-- 'Matrix' provides a 3x2 affine transform for pre-computing combined
-- transforms, applying to points, and composing transform pipelines.
module GBVector.Transform
  ( -- * Translation
    translate,

    -- * Rotation
    rotate,
    rotateAround,

    -- * Scaling
    scale,
    scaleXY,

    -- * Skewing
    skewX,
    skewY,

    -- * Affine Matrix
    Matrix (..),
    identity,
    composeMatrix,
    applyMatrix,
    translateM,
    rotateM,
    scaleM,
    scaleXYM,
    skewXM,
    skewYM,
  )
where

import GBVector.Element (Element (..))
import GBVector.Types (V2 (..))

-- ---------------------------------------------------------------------------
-- Translation
-- ---------------------------------------------------------------------------

-- | Translate an element by @(dx, dy)@.
translate :: Double -> Double -> Element -> Element
translate = ETranslate

-- ---------------------------------------------------------------------------
-- Rotation
-- ---------------------------------------------------------------------------

-- | Rotate an element by the given angle in degrees (around the origin).
rotate :: Double -> Element -> Element
rotate = ERotate

-- | Rotate an element by the given angle in degrees around a center point.
rotateAround :: Double -> V2 -> Element -> Element
rotateAround = ERotateAround

-- ---------------------------------------------------------------------------
-- Scaling
-- ---------------------------------------------------------------------------

-- | Uniform scale by a factor.
scale :: Double -> Element -> Element
scale s = EScale s s

-- | Non-uniform scale by @(sx, sy)@.
scaleXY :: Double -> Double -> Element -> Element
scaleXY = EScale

-- ---------------------------------------------------------------------------
-- Skewing
-- ---------------------------------------------------------------------------

-- | Skew along the X axis by the given angle in degrees.
skewX :: Double -> Element -> Element
skewX = ESkewX

-- | Skew along the Y axis by the given angle in degrees.
skewY :: Double -> Element -> Element
skewY = ESkewY

-- ---------------------------------------------------------------------------
-- Affine Matrix
-- ---------------------------------------------------------------------------

-- | A 3x2 affine transformation matrix.
--
-- Represents the transform:
--
-- @
--   | a c tx |   | x |   | a*x + c*y + tx |
--   | b d ty | * | y | = | b*x + d*y + ty |
--   | 0 0  1 |   | 1 |   |              1 |
-- @
data Matrix = Matrix
  { -- | Scale X / cos component.
    matA :: !Double,
    -- | Shear Y / sin component.
    matB :: !Double,
    -- | Shear X / negative sin component.
    matC :: !Double,
    -- | Scale Y / cos component.
    matD :: !Double,
    -- | Translation X.
    matTx :: !Double,
    -- | Translation Y.
    matTy :: !Double
  }
  deriving (Show, Eq)

-- | The identity matrix (no transformation).
identity :: Matrix
identity = Matrix 1 0 0 1 0 0

-- | Compose two matrices: @composeMatrix m1 m2@ applies @m1@ after @m2@.
composeMatrix :: Matrix -> Matrix -> Matrix
composeMatrix (Matrix a1 b1 c1 d1 tx1 ty1) (Matrix a2 b2 c2 d2 tx2 ty2) =
  Matrix
    { matA = a1 * a2 + c1 * b2,
      matB = b1 * a2 + d1 * b2,
      matC = a1 * c2 + c1 * d2,
      matD = b1 * c2 + d1 * d2,
      matTx = a1 * tx2 + c1 * ty2 + tx1,
      matTy = b1 * tx2 + d1 * ty2 + ty1
    }

-- | Apply a matrix to a 2D point.
applyMatrix :: Matrix -> V2 -> V2
applyMatrix (Matrix a b c d tx ty) (V2 x y) =
  V2 (a * x + c * y + tx) (b * x + d * y + ty)

-- | Translation matrix.
translateM :: Double -> Double -> Matrix
translateM = Matrix 1 0 0 1

-- | Rotation matrix (angle in degrees, around the origin).
rotateM :: Double -> Matrix
rotateM deg =
  let rad = deg * degreesToRadians
      cosR = cos rad
      sinR = sin rad
   in Matrix cosR sinR (negate sinR) cosR 0 0

-- | Uniform scale matrix.
scaleM :: Double -> Matrix
scaleM s = Matrix s 0 0 s 0 0

-- | Non-uniform scale matrix.
scaleXYM :: Double -> Double -> Matrix
scaleXYM sx sy = Matrix sx 0 0 sy 0 0

-- | Skew-X matrix (angle in degrees).
skewXM :: Double -> Matrix
skewXM deg = Matrix 1 0 (tan (deg * degreesToRadians)) 1 0 0

-- | Skew-Y matrix (angle in degrees).
skewYM :: Double -> Matrix
skewYM deg = Matrix 1 (tan (deg * degreesToRadians)) 0 1 0 0

-- ---------------------------------------------------------------------------
-- Internal
-- ---------------------------------------------------------------------------

-- | Conversion factor from degrees to radians.
degreesToRadians :: Double
degreesToRadians = pi / 180.0
