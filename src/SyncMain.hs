import Control.Monad (when)
import Options.Applicative
import Data.Semigroup ((<>))
import qualified Tracab as Tcb
import qualified F24
import qualified NeedlemanWunsch as NW
import qualified Csvs as CSV
import qualified Scoring


-- Command line parsing machinery
data Options = Options {
    tcbMetaFile :: String,
    tcbDataFile :: String,
    f7File      :: String,
    f24File     :: String,
    outputFile  :: String,
    timeOnly    :: Bool,
    showsync    :: Bool,
    cScale      :: Double,
    lScale      :: Double,
    pScale      :: Double,
    bScale      :: Double,    
    eventCSV    :: String,
    frameCSV    :: String
} deriving Show

options :: Parser Options
options = Options
    <$> argument str (metavar "TRACAB-META")
    <*> argument str (metavar "TRACAB-DATA")
    <*> argument str (metavar "F7")
    <*> argument str (metavar "F24")
    <*> argument str (metavar "OUTPUT")
    <*> switch (long "time-only" <> short 't' <> help "Sync only by time")
    <*> switch (long "show-sync" <> short 's' <> help "Print human-readable sync on screen")
    <*> option auto (long "scale-clock" <> short 'c' <> value 1 <> metavar "X" <> help "Clock difference resulting in unit penalty [s, default 1]")
    <*> option auto (long "scale-location" <> short 'l' <> value 5 <> metavar "X" <> help "Location difference resulting in unit penalty [m, default 5]")
    <*> option auto (long "scale-player" <> short 'p' <> value 1 <> metavar "X" <> help "Player-ball distance resulting in unit penalty [m, default 1]")
    <*> option auto (long "scale-ball" <> short 'b' <> value 5 <> metavar "X" <> help "Penalty for syncing to dead-ball frame [default 5]")
    <*> strOption (long "event-csv" <> short 'e' <> value "" <> metavar "FILEPATH" <> help "Location to save F24 events CSV")
    <*> strOption (long "frame-csv" <> short 'f' <> value "" <> metavar "FILEPATH" <> help "Location to save Tracab frames CSV")

parseOptions :: IO Options
parseOptions = let desc = "Synchronise Tracab and F24 data."
                   hdr = "Copyright (c) 2018-2019 Allan Clark and Marek Kwiatkowski."
               in execParser $ info (options <**> helper) (fullDesc <> progDesc desc <> header hdr)

main :: IO ()
main = do
    opts <- parseOptions
    tbMeta <- Tcb.parseMetaFile (tcbMetaFile opts)
    tbData <- Tcb.parseDataFile tbMeta (tcbDataFile opts)
    f24Meta <- F24.parseMetaFile (f7File opts)
    f24Raw <- F24.loadGameFromFile (f24File opts)
    let f24Data = F24.convertGameCoordinates tbMeta tbData f24Raw

    -- Select events to sync in both halves
    let syncable pid e = F24.period_id e == pid && F24.isOTB e
    let events1 = filter (syncable 1) (F24.events f24Data)
    let events2 = filter (syncable 2) (F24.events f24Data)

    -- Select frames to sync to
    let p1start = (Tcb.startFrame . head . Tcb.periods) tbMeta
    let p2start = (Tcb.startFrame . head . tail . Tcb.periods) tbMeta
    let p1end = (Tcb.endFrame . head . Tcb.periods) tbMeta
    let p2end = (Tcb.endFrame . head . tail . Tcb.periods) tbMeta
    let frames1 = filter (\f -> (Tcb.frameId f <= p1end) && (Tcb.frameId f >= p1start)) tbData
    let frames2 = filter (\f -> (Tcb.frameId f <= p2end) && (Tcb.frameId f >= p2start)) tbData

    -- The penalty for leaving frames unaligned needs to be small.
    -- Conversely, leaving events unaligned should be costly.
    -- Note that the score for a Match is negative on the log-density scale.
    let gapl f = -10.0    -- Leaves a frame unaligned for p < exp(-10) = 4.5e-5
    let gapr e = -1000.0

    -- Build the scoring function as requested by the user.
    -- FIXME: Combine functions without mentioning arguments.
    let to = timeOnly opts
    let scoreClock = Scoring.clockScore (cScale opts)
    let scoreLocation e f = if to then 0 else Scoring.locationScore (100 * lScale opts) e f
    let scorePlayer e f = if to then 0 else Scoring.playerScore (F24.shirtNumbers f24Meta) (100 * pScale opts) e f
    let scoreBall e f = if to then 0 else Scoring.ballStatusScore (bScale opts) e f
    let sim e f = scoreClock e f + scoreLocation e f + scorePlayer e f + scoreBall e f

    -- Align!
    let sync1 = NW.align events1 frames1 sim gapl gapr
    let sync2 = NW.align events2 frames2 sim gapl gapr
    CSV.alignment2Csv (NW.joinAlignments sync1 sync2) (outputFile opts)

    -- Print sync on screen if requested
    when (showsync opts) $ putStr (show sync1)
    when (showsync opts) $ putStr (show sync2)

    -- Write event and frame CSVs if requested
    when (eventCSV opts /= "") $ CSV.events2Csv (events1 ++ events2) (eventCSV opts)
    when (frameCSV opts /= "") $ CSV.frames2Csv (frames1 ++ frames2) (frameCSV opts)
