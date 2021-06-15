{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Lightningometry.Annotate (
    RouteData (..),
    hops,
    AnnotatedGraph (..),
    getAnnotatedGraph,
) where

import Data.Aeson (ToJSON (toJSON), object, (.=))
import Data.Foldable (foldl', toList)
import Data.Map.Lazy (Map)
import qualified Data.Map.Lazy as Map
import Data.Map.Merge.Lazy as Map
import Data.Maybe (mapMaybe)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import Data.Word (Word64)
import Lightningometry (Channel, NodeId)
import qualified Lightningometry as L
import Lightningometry.Graph (HalfEdge)
import qualified Lightningometry.Graph as G

data RouteData = RouteStart | RouteData Int Word64
    deriving (Eq, Show)

instance ToJSON RouteData where
    toJSON = \case
        RouteData routeHops routeCapacity ->
            object
                [ "tag" .= ("RouteData" :: Text)
                , "hops" .= routeHops
                , "capacity" .= routeCapacity
                ]
        RouteStart -> object ["tag" .= ("RouteStart" :: Text)]

hops :: RouteData -> Int
hops = \case
    RouteStart -> 0
    RouteData h _ -> h

data AnnotatedGraph = AnnotatedGraph
    { annotatedGraph :: Map NodeId (Map NodeId RouteData)
    , visitedNodes :: Set NodeId
    , radius :: Int
    }
    deriving (Eq, Show)

instance ToJSON AnnotatedGraph where
    toJSON graph =
        object
            [ "graph" .= annotatedGraph graph
            , "visited_nodes" .= visitedNodes graph
            , "radius" .= radius graph
            ]

getAnnotatedGraph ::
    Map NodeId [HalfEdge] ->
    Int ->
    [NodeId] ->
    AnnotatedGraph
getAnnotatedGraph nhoods maxLength =
    (!! maxLength) . iterate (annotateStep nhoods) . initialGraph

initialGraph :: [NodeId] -> AnnotatedGraph
initialGraph ns =
    AnnotatedGraph
        { annotatedGraph = Map.fromList $ mkMap <$> ns
        , visitedNodes = Set.fromList ns
        , radius = 0
        }
  where
    mkMap node = (node, Map.singleton node RouteStart)

-- | Analyze the boundary of the input graph
annotateStep ::
    Map NodeId [HalfEdge] ->
    AnnotatedGraph ->
    AnnotatedGraph
annotateStep nhoods graph0 =
    AnnotatedGraph
        { annotatedGraph = foldl' updateGraph (annotatedGraph graph0) nextNeighborhood
        , visitedNodes = visited <> nextNeighborhood
        , radius = radius graph0 + 1
        }
  where
    visited = visitedNodes graph0
    nextNeighborhood =
        Set.fromList
            . fmap G.node
            . mconcat
            . mapMaybe (`Map.lookup` nhoods)
            $ toList visited

    updateGraph graph boundaryNode
        | Just nhood <- Map.lookup boundaryNode nhoods =
            foldl' updateGraphOnChannel graph $ G.channel <$> nhood
        | otherwise = graph

    updateGraphOnChannel graph channel =
        updateRouteData channel sourceNode sourceRouteData destRouteData
            . updateRouteData channel destNode destRouteData sourceRouteData
            $ graph
      where
        sourceNode = L.source channel
        sourceRouteData = Map.findWithDefault mempty sourceNode graph
        destNode = L.destination channel
        destRouteData = Map.findWithDefault mempty destNode graph

    updateRouteData channel theNode nodeRouteData neighborRouteData =
        Map.insert theNode (upgradeRouteData channel nodeRouteData neighborRouteData)

    upgradeRouteData ::
        Channel ->
        -- | Node route data
        Map NodeId RouteData ->
        -- | Neighbor route data
        Map NodeId RouteData ->
        Map NodeId RouteData
    upgradeRouteData channel =
        Map.merge
            Map.preserveMissing
            (Map.mapMissing . const $ extendRouteData channel)
            (Map.zipWithMatched . const $ compareRoutes channel)

    compareRoutes channel nodeRouteData neighborRouteData =
        case nodeRouteData of
            routeData@(RouteData _nodeHops nodeCapacity) -> case neighborRouteData of
                RouteData neighborHops neighborCapacity
                    | modifiedCapacity > nodeCapacity ->
                        RouteData (neighborHops + 1) modifiedCapacity
                  where
                    modifiedCapacity = min (L.capacity channel) neighborCapacity
                RouteStart
                    | L.capacity channel > nodeCapacity ->
                        RouteData 1 (L.capacity channel)
                _ -> routeData
            RouteStart -> RouteStart

    extendRouteData ch = \case
        RouteStart -> RouteData 1 (L.capacity ch)
        RouteData neighborHops neighborCapacity ->
            RouteData (neighborHops + 1) $ min neighborCapacity (L.capacity ch)
