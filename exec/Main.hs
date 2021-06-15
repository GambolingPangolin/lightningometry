{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Exception (throwIO)
import Control.Monad.IO.Class (liftIO)
import qualified Data.ByteString as BS
import Data.Map.Lazy (Map)
import qualified Data.Text.IO as TIO
import Data.Yaml (decodeFileEither, encode)
import Lightningometry (Channel, NodeId, Payee, nodeId, unChannels)
import Lightningometry.Analyze (getCapacityFrontier, minimizeWeightedHops, onlyLargestReachSets)
import Lightningometry.Annotate (RouteData, annotatedGraph, getAnnotatedGraph)
import Lightningometry.Graph (getNeighborhoods)
import Lightningometry.Options (OutputStyle (..), getConfig)
import qualified Lightningometry.Options as O
import Lightningometry.Report (report)

main :: IO ()
main = do
    cliConf <- getConfig
    channels <- fmap unChannels . throwError =<< decodeFileEither (O.channelFile cliConf)
    payees <- throwError =<< decodeFileEither (O.payeesFile cliConf)
    let analysis = runAnalysis channels payees (O.hopLimit cliConf)
    case O.outputStyle cliConf of
        OutputYaml -> (liftIO . BS.putStr . encode) analysis
        OutputReport -> TIO.putStrLn $ report analysis
  where
    throwError = either throwIO pure

runAnalysis ::
    [Channel] ->
    [Payee] ->
    Int ->
    Map NodeId (Map NodeId RouteData)
runAnalysis channels payees hopLimit =
    minimizeWeightedHops payees
        . getCapacityFrontier
        . onlyLargestReachSets
        . annotatedGraph
        $ getAnnotatedGraph (getNeighborhoods channels) hopLimit (nodeId <$> payees)
