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
    timeOnly    :: Bool
} deriving Show

options :: Parser Options
options = Options
       <$> argument str (metavar "TRACAB-META")
       <*> argument str (metavar "TRACAB-DATA")
       <*> argument str (metavar "F7")
       <*> argument str (metavar "F24")
       <*> argument str (metavar "OUTPUT")
       <*> switch (long "time-only" <> short 't' <> help "Sync only by time")

parseOptions :: IO Options
parseOptions = let desc = "Synchronise Tracab and F24 data."
                   hdr = "Copyright (c) 2018 Allan Clark and Marek Kwiatkowski."
               in execParser $ info (options <**> helper) (fullDesc <> progDesc desc <> header hdr)

main :: IO ()
main = do
    opts <- parseOptions
    tbMeta <- Tcb.parseMetaFile (tcbMetaFile opts)
    tbData <- Tcb.parseDataFile tbMeta (tcbDataFile opts)
    f24Meta <- F24.parseMetaFile (f7File opts)
    f24Raw <- F24.loadGameFromFile (f24File opts)
    let f24Data = F24.convertGameCoordinates tbMeta tbData f24Raw

    -- Select first and second half events and frames
    let events1 = filter (\e -> F24.period_id e == 1) (F24.events f24Data)
    let events2 = filter (\e -> F24.period_id e == 2) (F24.events f24Data)

    let p1start = (Tcb.startFrame . head . Tcb.periods) tbMeta
    let p2start = (Tcb.startFrame . head . tail . Tcb.periods) tbMeta
    let p1end = (Tcb.endFrame . head . Tcb.periods) tbMeta
    let p2end = (Tcb.endFrame . head . tail . Tcb.periods) tbMeta
    let frames1 = filter (\f -> (Tcb.frameId f <= p1end) && (Tcb.frameId f >= p1start)) tbData
    let frames2 = filter (\f -> (Tcb.frameId f <= p2end) && (Tcb.frameId f >= p2start)) tbData

    -- So if you want to do some smoothing using matrices for the frame data then
    let frameMatrices = Tcb.translateFrames frames2
    -- If you want just a list of matrices then
    let tracabMatrices = map Tcb.positions frameMatrices

    -- The penalty for leaving frames unaligned needs to be small.
    -- Conversely, leaving events unaligned should be costly.
    -- Note that the score for a Match is negative on the log-density scale.
    let gapl = \f -> (-10.0)    -- Leaves a frame unaligned for p < exp(-10) = 4.5e-5
    let gapr = \e -> (-1000.0)
    let sim = case timeOnly opts of
          True  -> Scoring.clockScore 1.0
          False -> Scoring.totalScore (F24.shirtNumbers f24Meta) 1.0

    -- Align!
    let sync1 = NW.align events1 frames1 sim gapl gapr
    let sync2 = NW.align events2 frames2 sim gapl gapr
    CSV.alignment2Csv (NW.joinAlignments sync1 sync2) (outputFile opts)
