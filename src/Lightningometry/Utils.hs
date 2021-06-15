module Lightningometry.Utils (
    showText,
) where

import Data.Text (Text)
import qualified Data.Text as Text

showText :: Show a => a -> Text
showText = Text.pack . show
