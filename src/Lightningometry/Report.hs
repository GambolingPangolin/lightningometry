{-# LANGUAGE OverloadedStrings #-}

module Lightningometry.Report (
    report,
) where

import Data.Map.Lazy (Map)
import qualified Data.Map.Lazy as Map
import Data.Text (Text)
import qualified Data.Text as Text
import Lightningometry (NodeId)
import Lightningometry.Annotate (RouteData)
import Lightningometry.Utils (showText)

report :: Map NodeId (Map NodeId RouteData) -> Text
report results =
    Text.unlines . mconcat $
        [
            [ "Result count: " <> showText (Map.size results)
            , "Best nodes:"
            ]
        , ("* " <>) <$> Map.keys results
        ]
