{-# LANGUAGE OverloadedStrings #-}

module Lightningometry (
    NodeId,
    Channels (..),
    Channel (..),
    Payee (..),
) where

import Data.Aeson (FromJSON (parseJSON), ToJSON (toJSON), object, withObject, (.:), (.=))
import Data.Text (Text)
import Data.Time (UTCTime)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime, utcTimeToPOSIXSeconds)
import Data.Word (Word64)

type NodeId = Text

data Channel = Channel
    { source :: NodeId
    , destination :: NodeId
    , active :: Bool
    , capacity :: Word64
    , lastUpdate :: UTCTime
    }
    deriving (Eq, Show, Ord)

instance FromJSON Channel where
    parseJSON = withObject "Channel" $ \obj ->
        Channel
            <$> obj .: "source"
            <*> obj .: "destination"
            <*> obj .: "active"
            <*> obj .: "satoshis"
            <*> (posixSecondsToUTCTime <$> obj .: "last_update")

instance ToJSON Channel where
    toJSON ch =
        object
            [ "source" .= source ch
            , "destination" .= destination ch
            , "active" .= active ch
            , "satoshis" .= capacity ch
            , "last_update" .= utcTimeToPOSIXSeconds (lastUpdate ch)
            ]

newtype Channels = Channels {unChannels :: [Channel]}
    deriving (Eq, Show)

instance FromJSON Channels where
    parseJSON = withObject "Channels" $ fmap Channels . (.: "channels")

data Payee = Payee
    { nodeId :: NodeId
    , weight :: Double
    }
    deriving (Eq, Show)

instance FromJSON Payee where
    parseJSON = withObject "Payee" $ \obj ->
        Payee
            <$> obj .: "node_id"
            <*> obj .: "weight"
