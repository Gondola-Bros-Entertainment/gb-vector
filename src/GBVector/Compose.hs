-- | Composition combinators and t'Document' construction.
--
-- 'Element' has 'Semigroup' and 'Monoid' instances via 'EGroup'.
module GBVector.Compose
  ( -- * Grouping
    group,
    empty,

    -- * Document Construction
    document,
    documentWithViewBox,

    -- * Background
    background,

    -- * Optimization
    optimizeElement,
  )
where

import GBVector.Color (Color)
import GBVector.Element
  ( Document (..),
    Element (..),
    Fill (..),
  )
import GBVector.Types (ViewBox (..))

-- ---------------------------------------------------------------------------
-- Grouping
-- ---------------------------------------------------------------------------

-- | Group a list of elements.
group :: [Element] -> Element
group [] = EEmpty
group [single] = single
group elements = EGroup elements

-- | The empty element. Renders nothing.
empty :: Element
empty = EEmpty

-- ---------------------------------------------------------------------------
-- Document Construction
-- ---------------------------------------------------------------------------

-- | Create a document with the given width, height, and root element.
document :: Double -> Double -> Element -> Document
document w h el =
  Document
    { docWidth = w,
      docHeight = h,
      docViewBox = Nothing,
      docElement = el
    }

-- | Create a document with explicit viewBox.
documentWithViewBox :: Double -> Double -> ViewBox -> Element -> Document
documentWithViewBox w h vb el =
  Document
    { docWidth = w,
      docHeight = h,
      docViewBox = Just vb,
      docElement = el
    }

-- ---------------------------------------------------------------------------
-- Background
-- ---------------------------------------------------------------------------

-- | Add a background rectangle behind the given element.
background :: Double -> Double -> Color -> Element -> Element
background w h c el = EGroup [EFill (SolidFill c) (ERect w h), el]

-- ---------------------------------------------------------------------------
-- Optimization
-- ---------------------------------------------------------------------------

-- | Simplify an element tree by collapsing redundant wrappers.
--
-- * Merges nested translates: @translate a b (translate c d x)@ becomes
--   @translate (a+c) (b+d) x@.
-- * Merges nested scales: @scale a b (scale c d x)@ becomes
--   @scale (a*c) (b*d) x@.
-- * Removes identity translates (@translate 0 0@).
-- * Removes identity scales (@scale 1 1@).
-- * Removes identity opacity (@opacity 1@).
-- * Flattens singleton groups.
-- * Removes EEmpty from groups.
optimizeElement :: Element -> Element
optimizeElement el = case el of
  -- Collapse nested translates
  ETranslate dx1 dy1 (ETranslate dx2 dy2 child) ->
    optimizeElement (ETranslate (dx1 + dx2) (dy1 + dy2) child)
  -- Collapse nested scales
  EScale sx1 sy1 (EScale sx2 sy2 child) ->
    optimizeElement (EScale (sx1 * sx2) (sy1 * sy2) child)
  -- Remove identity translate
  ETranslate dx dy child
    | dx == 0 && dy == 0 -> optimizeElement child
    | otherwise -> ETranslate dx dy (optimizeElement child)
  -- Remove identity scale
  EScale sx sy child
    | sx == 1 && sy == 1 -> optimizeElement child
    | otherwise -> EScale sx sy (optimizeElement child)
  -- Remove identity opacity
  EOpacity a child
    | a >= 1 -> optimizeElement child
    | otherwise -> EOpacity a (optimizeElement child)
  -- Flatten groups
  EGroup children ->
    let optimized = filter (not . isEmpty) (map optimizeElement children)
     in case optimized of
          [] -> EEmpty
          [single] -> single
          multiple -> EGroup multiple
  -- Recurse into wrappers
  EFill f child -> EFill f (optimizeElement child)
  EStroke c w child -> EStroke c w (optimizeElement child)
  EStrokeEx cfg child -> EStrokeEx cfg (optimizeElement child)
  EFillRule r child -> EFillRule r (optimizeElement child)
  ERotate deg child -> ERotate deg (optimizeElement child)
  ERotateAround deg center child -> ERotateAround deg center (optimizeElement child)
  ESkewX deg child -> ESkewX deg (optimizeElement child)
  ESkewY deg child -> ESkewY deg (optimizeElement child)
  EClip clipEl child -> EClip (optimizeElement clipEl) (optimizeElement child)
  EMask maskEl child -> EMask (optimizeElement maskEl) (optimizeElement child)
  EFilter fk child -> EFilter fk (optimizeElement child)
  EWithId elId child -> EWithId elId (optimizeElement child)
  ETitle t child -> ETitle t (optimizeElement child)
  EDesc d child -> EDesc d (optimizeElement child)
  -- Leaves pass through
  other -> other

-- | Check if an element is empty.
isEmpty :: Element -> Bool
isEmpty EEmpty = True
isEmpty _ = False
