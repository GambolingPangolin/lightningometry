module Lightningometry.Analyze (
    onlyLargestReachSets,
    getCapacityFrontier,
    minimizeWeightedHops,
) where

import Control.Arrow ((&&&))
import Data.Map.Lazy (Map)
import qualified Data.Map.Lazy as Map
import Data.Ord (Down (Down))
import Lightningometry (NodeId, Payee)
import qualified Lightningometry as L
import Lightningometry.Annotate (RouteData (..))

{- | Create a map consisting entirely of those nodes which have route data for
 the maximum number of source nodes.
-}
onlyLargestReachSets :: Map NodeId (Map NodeId RouteData) -> Map NodeId (Map NodeId RouteData)
onlyLargestReachSets = maximizeCriterion Map.size 1

{- | Eliminate entries which are dominated by other entries in the map in
 the sense that the capacity of one entry is lower for each source node.
-}
getCapacityFrontier :: Map NodeId (Map NodeId RouteData) -> Map NodeId (Map NodeId RouteData)
getCapacityFrontier = Map.foldlWithKey examineMap mempty
  where
    examineMap frontier thisNode thisRouteData
        | null frontier = Map.singleton thisNode thisRouteData
        | not $ any (thisRouteData `isDominatedBy`) frontier =
            Map.insert thisNode thisRouteData $
                Map.filter (not . (`isDominatedBy` thisRouteData)) frontier
        | otherwise = frontier

isDominatedBy ::
    Ord k =>
    Map k RouteData ->
    Map k RouteData ->
    Bool
isDominatedBy routeData1 = and . Map.intersectionWith dominatesByCapacity routeData1

dominatesByCapacity :: RouteData -> RouteData -> Bool
dominatesByCapacity RouteStart _ = False
dominatesByCapacity _ RouteStart = True
dominatesByCapacity (RouteData h1 c1) (RouteData h2 c2) =
    c1 < c2 || c1 == c2 && h2 <= h1

-- | This adds a floating point notion of hops (for weighted hop sums) with a maximum value: 'NoRoute'
data HopCount = NoRoute | Hops Double
    deriving (Eq, Show)

instance Ord HopCount where
    compare NoRoute _ = GT
    compare _ NoRoute = LT
    compare (Hops h1) (Hops h2) = compare h1 h2

-- | Use the payee weights to retain those results with the minimum weighted hop value
minimizeWeightedHops ::
    [Payee] ->
    Map NodeId (Map NodeId RouteData) ->
    Map NodeId (Map NodeId RouteData)
minimizeWeightedHops payees = maximizeCriterion weightedHops (Down NoRoute)
  where
    weightedHops = Down . Hops . sum . Map.intersectionWith mkHops hopWeights
    mkHops _ RouteStart = 0
    mkHops w (RouteData h _) = w * fromIntegral h / totalWeight

    hopWeights = Map.fromList $ (L.nodeId &&& L.weight) <$> payees
    totalWeight = sum $ L.weight <$> payees

maximizeCriterion ::
    (Ord k, Ord v) =>
    (a -> v) ->
    v ->
    Map k a ->
    Map k a
maximizeCriterion measure val0 = snd . Map.foldlWithKey examineMap (val0, mempty)
  where
    examineMap acc@(n, routeData) thisNode thisRouteData
        | measure thisRouteData > n = (measure thisRouteData, Map.singleton thisNode thisRouteData)
        | measure thisRouteData == n = (n, Map.insert thisNode thisRouteData routeData)
        | otherwise = acc
