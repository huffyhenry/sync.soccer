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

clockScore :: Double -> Double -> F24.Event Tracab.Coordinates -> Tracab.Frame -> Double
clockScore scale offset e f =
    let seconds = fromIntegral $ 60 * (F24.min e) + (F24.sec e)
        dist = abs $ seconds - (fromJust $ Tracab.clock f)
    in logDensity Gaussian.standard ((dist - offset) / scale)

locationScore :: Double -> F24.Event Tracab.Coordinates -> Tracab.Frame -> Double
locationScore scale e f =
    let eX = (Tracab.x . fromJust . F24.coordinates) e
        eY = (Tracab.y . fromJust . F24.coordinates) e
        fX = (Tracab.x . Tracab.coordinates . Tracab.ballPosition) f
        fY = (Tracab.y . Tracab.coordinates . Tracab.ballPosition) f
        xDist = fromIntegral $ eX - fX
        yDist = fromIntegral $ eY - fY
        dist = sqrt $ xDist**2.0 + yDist**2.0
    in logDensity Gaussian.standard (dist / scale)

totalScore :: Double -> F24.Event Tracab.Coordinates -> Tracab.Frame -> Double
totalScore offset e f = (clockScore 1.0 offset e f) + (locationScore 100.0 e f)

-- Command line parsing machinery
data Options = Options {
    tcbMetaFile :: String,
    tcbDataFile :: String,
    f24File     :: String,
    outputFile  :: String,
    timeOnly    :: Bool
} deriving Show

options :: Parser Options
options = Options
       <$> argument str (metavar "TRACAB-META")
       <*> argument str (metavar "TRACAB-DATA")
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
    f24Raw <- F24.loadGameFromFile (f24File opts)
    let f24Data = F24.convertGameCoordinates tbMeta tbData f24Raw
    let events = filter (\e -> F24.period_id e == 1) (F24.events f24Data)
    let p1start = (Tracab.startFrame . head . Tracab.periods) tbMeta
    let p1end = (Tracab.endFrame . head . Tracab.periods) tbMeta
    let frames = filter (\f -> (Tracab.frameId f <= p1end) && (Tracab.frameId f >= p1start)) tbData

    -- Take just the first ~X minutes to stay under 16GB RAM for now.
    let minutes = 20
    let events2 = filter (\e -> F24.min e < minutes) events
    let frames2 = take (25*60*(minutes + 5)) frames

    -- The penalty for leaving frames unaligned needs to be small.
    -- Conversely, leaving events unaligned should be costly.
    -- Note that the score for a Match is negative on the log-density scale.
    let gapl = \f -> (-10.0)    -- Leaves a frame unaligned for p < exp(-10) = 4.5e-5
    let gapr = \e -> (-1000.0)
    let sim = if timeOnly opts then clockScore 1.0 0.0 else totalScore 0.0
    let sync = NW.align events2 frames2 sim gapl gapr
    putStrLn $ show sync
    putStr $ (show $ length frames2) ++ " frames, "
    putStr $ (show $ length events2) ++ " events, "
    putStrLn $ "total score " ++ show (NW.alignmentScore sim gapl gapr sync) ++ "."

    -- Write parsed data to CSVs for animation
    CSV.frames2Csv frames2 "data/csv/frames.csv"
    CSV.events2Csv events2 "data/csv/events.csv"
    CSV.alignment2Csv sync (outputFile opts)
