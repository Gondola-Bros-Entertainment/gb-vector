-- | Style smart constructors.
--
-- Each function wraps an 'Element' in a style constructor.
-- Compose naturally:
--
-- @fill red $ stroke black 2 $ circle 30@
module GBVector.Style
  ( -- * Fill
    fill,
    fillColor,
    fillGradient,
    fillNone,
    fillRule,

    -- * Stroke
    stroke,
    strokeEx,

    -- * Opacity
    opacity,

    -- * Clipping & Masking
    clip,
    mask,

    -- * Filters
    blur,
    dropShadow,

    -- * Identity & References
    withId,
    use,
    raw,

    -- * Accessibility
    title,
    desc,
  )
where

import Data.Text (Text)
import GBVector.Color (Color)
import GBVector.Element
  ( Element (..),
    Fill (..),
    FilterKind (..),
    Gradient,
    StrokeConfig,
  )
import GBVector.Types (FillRule)

-- ---------------------------------------------------------------------------
-- Fill
-- ---------------------------------------------------------------------------

-- | Fill an element with a solid color.
fill :: Color -> Element -> Element
fill c = EFill (SolidFill c)

-- | Fill an element with a solid color (alias for 'fill').
fillColor :: Color -> Element -> Element
fillColor = fill

-- | Fill an element with a gradient.
fillGradient :: Gradient -> Element -> Element
fillGradient g = EFill (GradientFill g)

-- | Remove fill from an element.
fillNone :: Element -> Element
fillNone = EFill NoFill

-- | Set the fill rule for an element.
fillRule :: FillRule -> Element -> Element
fillRule = EFillRule

-- ---------------------------------------------------------------------------
-- Stroke
-- ---------------------------------------------------------------------------

-- | Stroke an element with a color and width.
stroke :: Color -> Double -> Element -> Element
stroke = EStroke

-- | Stroke an element with full configuration.
strokeEx :: StrokeConfig -> Element -> Element
strokeEx = EStrokeEx

-- ---------------------------------------------------------------------------
-- Opacity
-- ---------------------------------------------------------------------------

-- | Set the opacity of an element (0 = transparent, 1 = opaque).
opacity :: Double -> Element -> Element
opacity = EOpacity

-- ---------------------------------------------------------------------------
-- Clipping & Masking
-- ---------------------------------------------------------------------------

-- | Clip an element to the shape of a clip path.
-- @clip clipShape content@ clips @content@ to the area defined by @clipShape@.
clip :: Element -> Element -> Element
clip = EClip

-- | Mask an element with another element's luminance.
-- @mask maskElement content@ masks @content@ using @maskElement@.
mask :: Element -> Element -> Element
mask = EMask

-- ---------------------------------------------------------------------------
-- Filters
-- ---------------------------------------------------------------------------

-- | Apply a Gaussian blur with the given standard deviation.
blur :: Double -> Element -> Element
blur stdDev = EFilter (FilterBlur stdDev)

-- | Apply a drop shadow with offset, blur, and color.
dropShadow :: Double -> Double -> Double -> Color -> Element -> Element
dropShadow dx dy blurRadius shadowColor = EFilter (FilterDropShadow dx dy blurRadius shadowColor)

-- ---------------------------------------------------------------------------
-- Identity & References
-- ---------------------------------------------------------------------------

-- | Assign an SVG @id@ attribute to an element.
withId :: Text -> Element -> Element
withId = EWithId

-- | Reference a previously defined element by its @id@.
use :: Text -> Element
use = EUse

-- | Inject raw SVG markup.
raw :: Text -> Element
raw = ERaw

-- ---------------------------------------------------------------------------
-- Accessibility
-- ---------------------------------------------------------------------------

-- | Add a @\<title\>@ element for accessibility. Screen readers use this
-- as the accessible name of the element.
title :: Text -> Element -> Element
title = ETitle

-- | Add a @\<desc\>@ element for accessibility. Provides a longer
-- description of the element for assistive technology.
desc :: Text -> Element -> Element
desc = EDesc
