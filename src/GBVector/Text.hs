-- | Text element construction.
--
-- @
-- translate 50 50 $ fill black $ text "Hello, world!"
-- @
module GBVector.Text
  ( -- * Text Elements
    text,
    textAt,
    textWithConfig,

    -- * Configuration
    defaultTextConfig,
    fontSize,
    fontFamily,
    bold,
    italic,
    anchor,
  )
where

import Data.Text (Text)
import qualified Data.Text as T
import GBVector.Element
  ( Element (..),
    TextAnchor (..),
    TextConfig (..),
  )

-- ---------------------------------------------------------------------------
-- Text Elements
-- ---------------------------------------------------------------------------

-- | Create a text element with default configuration.
text :: Text -> Element
text = EText defaultTextConfig

-- | Create a text element at a specific position.
textAt :: Double -> Double -> Text -> Element
textAt x y content = ETranslate x y (EText defaultTextConfig content)

-- | Create a text element with full configuration.
textWithConfig :: TextConfig -> Text -> Element
textWithConfig = EText

-- ---------------------------------------------------------------------------
-- Configuration
-- ---------------------------------------------------------------------------

-- | Default text configuration: 16px sans-serif, start-anchored, normal weight.
defaultTextConfig :: TextConfig
defaultTextConfig =
  TextConfig
    { textConfigFontFamily = T.pack "sans-serif",
      textConfigFontSize = defaultFontSize,
      textConfigAnchor = AnchorStart,
      textConfigBold = False,
      textConfigItalic = False
    }

-- | Set the font size.
fontSize :: Double -> TextConfig -> TextConfig
fontSize size config = config {textConfigFontSize = size}

-- | Set the font family.
fontFamily :: Text -> TextConfig -> TextConfig
fontFamily family config = config {textConfigFontFamily = family}

-- | Make the text bold.
bold :: TextConfig -> TextConfig
bold config = config {textConfigBold = True}

-- | Make the text italic.
italic :: TextConfig -> TextConfig
italic config = config {textConfigItalic = True}

-- | Set the text anchor.
anchor :: TextAnchor -> TextConfig -> TextConfig
anchor a config = config {textConfigAnchor = a}

-- ---------------------------------------------------------------------------
-- Constants
-- ---------------------------------------------------------------------------

defaultFontSize :: Double
defaultFontSize = 16
