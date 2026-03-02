-- | Gradient construction helpers.
--
-- Build gradients for use with 'fillGradient':
--
-- @fillGradient (linearGradient (V2 0 0) (V2 100 0) [stop 0 red, stop 1 blue]) $ rect 100 100@
module GBVector.Gradient
  ( -- * Gradient Construction
    linearGradient,
    radialGradient,

    -- * Stop Construction
    stop,
    stopWithOpacity,

    -- * Convenience
    evenStops,
  )
where

import GBVector.Color (Color)
import GBVector.Element
  ( Gradient (..),
    GradientStop (..),
  )
import GBVector.Types (SpreadMethod (..), V2)

-- ---------------------------------------------------------------------------
-- Gradient Construction
-- ---------------------------------------------------------------------------

-- | Create a linear gradient between two points with the given stops.
linearGradient :: V2 -> V2 -> [GradientStop] -> Gradient
linearGradient start end stops = LinearGradient start end stops SpreadPad

-- | Create a radial gradient with center, radius, and stops.
-- The focal point defaults to the center.
radialGradient :: V2 -> Double -> [GradientStop] -> Gradient
radialGradient center r stops = RadialGradient center r center stops SpreadPad

-- ---------------------------------------------------------------------------
-- Stop Construction
-- ---------------------------------------------------------------------------

-- | Create a gradient stop at the given offset with a color.
stop :: Double -> Color -> GradientStop
stop offset c = GradientStop offset c 1.0

-- | Create a gradient stop with explicit opacity.
stopWithOpacity :: Double -> Color -> Double -> GradientStop
stopWithOpacity = GradientStop

-- ---------------------------------------------------------------------------
-- Convenience
-- ---------------------------------------------------------------------------

-- | Distribute colors evenly across the gradient range @[0, 1]@.
evenStops :: [Color] -> [GradientStop]
evenStops [] = []
evenStops [c] = [stop 0 c]
evenStops colors =
  let count = length colors
      step = 1.0 / fromIntegral (count - 1)
   in zipWith (\i c -> stop (fromIntegral i * step) c) [0 :: Int ..] colors
