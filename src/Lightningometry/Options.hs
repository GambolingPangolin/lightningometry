module Lightningometry.Options (
    OutputStyle (..),
    CliConfig (..),
    getConfig,
) where

import Control.Applicative ((<**>))
import qualified Options.Applicative as Opt

data OutputStyle = OutputYaml | OutputReport
    deriving (Eq, Show)

data CliConfig = CliConfig
    { channelFile :: FilePath
    , payeesFile :: FilePath
    , hopLimit :: Int
    , outputStyle :: OutputStyle
    }
    deriving (Eq, Show)

getConfig :: IO CliConfig
getConfig = Opt.execParser $ Opt.info (opts <**> Opt.helper) desc
  where
    opts = CliConfig <$> optChannelData <*> optPayees <*> optHopLimit <*> optOutputStyle
    optChannelData =
        Opt.strOption $
            Opt.short 'c'
                <> Opt.long "channel-data"
                <> Opt.value defaultChannelDataFile
                <> helpWithDefault "Path to the file with channel data" defaultChannelDataFile
    optPayees =
        Opt.strOption $
            Opt.short 'p'
                <> Opt.long "payees"
                <> Opt.value defaultPayeeFile
                <> helpWithDefault "Path to the file with payee data" defaultPayeeFile

    optHopLimit =
        Opt.option Opt.auto $
            Opt.short 'n'
                <> Opt.long "hop-limit"
                <> Opt.value defaultHopLimit
                <> helpWithDefault "Maximum number of hops to rendezvous" (show defaultHopLimit)

    optOutputStyle =
        Opt.flag OutputYaml OutputReport $
            Opt.short 'r'
                <> Opt.long "report"
                <> Opt.help "If set, produce a textual report rather than a yaml document"

    defaultPayeeFile = "payees.json"
    defaultChannelDataFile = "channels.json"
    defaultHopLimit = 2

    helpWithDefault msg defaultValue = Opt.help $ msg <> " (default: " <> defaultValue <> ")"

    desc = Opt.progDesc "Find optimal peer sets for routing to a set of nodes"
