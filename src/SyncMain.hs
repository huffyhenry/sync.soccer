{-# LANGUAGE FlexibleInstances #-}

import qualified Data.IntMap as Map
import Data.Maybe (fromJust)
import System.Environment (getArgs)
import Statistics.Distribution (logDensity)
import Statistics.Distribution.Normal as Gaussian
import Options.Applicative
import Data.Semigroup ((<>))
import qualified Tracab
import qualified F24
import qualified NeedlemanWunsch as NW
import qualified Csvs as CSV
import qualified Control.Monad as Monad


clockScore :: Double -> F24.Event Tracab.Coordinates -> Tracab.Frame Tracab.Positions -> Double
clockScore scale e f =
    let seconds = fromIntegral $ 60 * (F24.min e) + (F24.sec e)
        dist = abs $ seconds - (fromJust $ Tracab.clock f)
    in logDensity Gaussian.standard (dist / scale)

locationScore :: Double -> F24.Event Tracab.Coordinates -> Tracab.Frame Tracab.Positions -> Double
locationScore scale e f =
    let eX = (Tracab.x . fromJust . F24.coordinates) e
        eY = (Tracab.y . fromJust . F24.coordinates) e
        ballCoordinates = Tracab.coordinates $ Tracab.ball $ Tracab.positions f
        fX = Tracab.x ballCoordinates
        fY = Tracab.y ballCoordinates
        xDist = fromIntegral $ eX - fX
        yDist = fromIntegral $ eY - fY
        dist = sqrt $ xDist**2.0 + yDist**2.0
    in logDensity Gaussian.standard (dist / scale)

ballStatusScore :: Double -> F24.Event Tracab.Coordinates -> Tracab.Frame Tracab.Positions -> Double
ballStatusScore scale _ f = case Tracab.mBallStatus $ Tracab.ball $ Tracab.positions f of
                               Nothing -> 0.0
                               Just Tracab.Alive -> scale
                               Just Tracab.Dead -> (-scale)

totalScore :: F24.Event Tracab.Coordinates -> Tracab.Frame Tracab.Positions -> Double
totalScore e f = (clockScore 1.0 e f) + (locationScore 100.0 e f) + (ballStatusScore 1.0 e f)

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
    tbMeta <- Tracab.parseMetaFile (tcbMetaFile opts)
    tbData <- Tracab.parseDataFile tbMeta (tcbDataFile opts)
    f24Meta <- F24.parseMetaFile (f7File opts)
    f24Raw <- F24.loadGameFromFile (f24File opts)
    let f24Data = F24.convertGameCoordinates tbMeta tbData f24Raw

    -- Select first and second half events and frames
    let events1 = filter (\e -> F24.period_id e == 1) (F24.events f24Data)
    let events2 = filter (\e -> F24.period_id e == 2) (F24.events f24Data)

    let p1start = (Tracab.startFrame . head . Tracab.periods) tbMeta
    let p2start = (Tracab.startFrame . head . tail . Tracab.periods) tbMeta
    let p1end = (Tracab.endFrame . head . Tracab.periods) tbMeta
    let p2end = (Tracab.endFrame . head . tail . Tracab.periods) tbMeta
    let frames1 = filter (\f -> (Tracab.frameId f <= p1end) && (Tracab.frameId f >= p1start)) tbData
    let frames2 = filter (\f -> (Tracab.frameId f <= p2end) && (Tracab.frameId f >= p2start)) tbData

    -- So if you want to do some smoothing using matrices for the frame data then
    let frameMatrices = Tracab.translateFrames frames2
    -- If you want just a list of matrices then
    let tracabMatrices = map Tracab.positions frameMatrices

    -- The penalty for leaving frames unaligned needs to be small.
    -- Conversely, leaving events unaligned should be costly.
    -- Note that the score for a Match is negative on the log-density scale.
    let gapl = \f -> (-10.0)    -- Leaves a frame unaligned for p < exp(-10) = 4.5e-5
    let gapr = \e -> (-1000.0)
    let sim = if timeOnly opts then clockScore 1.0 else totalScore

    -- Align!
    let sync1 = NW.align events1 frames1 sim gapl gapr
    let sync2 = NW.align events2 frames2 sim gapl gapr
    CSV.alignment2Csv (NW.joinAlignments sync1 sync2) (outputFile opts)
