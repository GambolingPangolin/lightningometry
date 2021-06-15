{-# LANGUAGE OverloadedStrings #-}

module Lightningometry.Graph (
    HalfEdge (..),
    oppositeNode,
    getNeighborhoods,
) where

import Data.Aeson (FromJSON (parseJSON), ToJSON (toJSON), object, withObject, (.:), (.=))
import Data.Foldable (foldl')
import Data.Map.Lazy (Map)
import qualified Data.Map.Lazy as Map
import Lightningometry (Channel (..), NodeId)

oppositeNode :: HalfEdge -> NodeId
oppositeNode edge
    | node edge == source (channel edge) = destination $ channel edge
    | otherwise = source $ channel edge

data HalfEdge = HalfEdge
    { channel :: Channel
    , node :: NodeId
    }
    deriving (Eq, Ord, Show)

instance FromJSON HalfEdge where
    parseJSON = withObject "HalfEdge" $ \obj ->
        HalfEdge <$> obj .: "channel" <*> obj .: "node_id"

instance ToJSON HalfEdge where
    toJSON halfEdge =
        object
            [ "channel" .= channel halfEdge
            , "node_id" .= node halfEdge
            ]

{- | Compute neighborhoods of nodes in the graph, represented by half edges on
 neighbor nodes
-}
getNeighborhoods :: [Channel] -> Map NodeId [HalfEdge]
getNeighborhoods = foldl' (flip addChannel) mempty . filter active
  where
    addChannel theChannel =
        (insert <$> source <*> destHalfEdge) theChannel
            . (insert <$> destination <*> sourceHalfEdge) theChannel
    destHalfEdge = HalfEdge <$> id <*> destination
    sourceHalfEdge = HalfEdge <$> id <*> source
    insert k v = Map.insertWith (<>) k [v]
