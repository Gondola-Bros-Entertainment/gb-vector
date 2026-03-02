-- | Composition combinators and 'Document' construction.
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
