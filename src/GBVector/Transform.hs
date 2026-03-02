-- | Transform smart constructors.
--
-- Each function wraps an 'Element' in a transform constructor.
-- Compose naturally:
--
-- @translate 100 100 $ rotate 45 $ scale 2 $ circle 30@
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
