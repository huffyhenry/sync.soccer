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

type ScoringFunction = F24.Event Tracab.Coordinates -> Tracab.Frame -> Double

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
    let flippedFirstHalf = Tracab.rightToLeftFirstHalf tbData
    let f24Data = F24.convertGameCoordinates flippedFirstHalf tbMeta f24Raw

    let allEvents = F24.events f24Data
    let allFrames = tbData
    let (firstHalfEvents, firstHalfFrames) = getEventsAndFrames 1 (Just 5) tbMeta allEvents allFrames
    -- This stackoverflow question explains why we need do `a par b pseq a+b`.
    -- https://stackoverflow.com/questions/4576734/why-do-we-need-seq-or-pseq-with-par-in-haskell


    let scoring = if timeOnly opts then clockScore 1.0 0.0 else totalScore 0.0
    let sync = alignEventsAndFrames scoring firstHalfEvents firstHalfFrames
    putStrLn $ show sync
    putStr $ (show $ length firstHalfEvents) ++ " frames, "
    putStr $ (show $ length firstHalfFrames) ++ " events, "
    putStrLn $ "total score " ++ show (NW.alignmentScore scoring gapl gapr sync) ++ "."

    -- Write parsed data to CSVs for animation
    CSV.frames2Csv firstHalfFrames "data/csv/frames.csv"
    CSV.events2Csv firstHalfEvents "data/csv/events.csv"
    CSV.alignment2Csv sync (outputFile opts)


getEventsAndFrames :: Int -> Maybe Int -> Tracab.Metadata -> [F24.Event Tracab.Coordinates] -> [Tracab.Frame] -> ([F24.Event Tracab.Coordinates], [Tracab.Frame])
getEventsAndFrames period mMinutes tbMeta allEvents allFrames =
    case mMinutes of
        Nothing ->
            (events, frames)
        Just minutes ->
            ( filter (\e -> F24.min e < minLimit) events
            , take (25*60*minLimit) frames
            )
            where
            -- TODO: This will obviously not work for extra-time periods, we're basically assuming
            -- that we'll only ever wish to restrict the minutes in the case that we're testing and
            -- hence we just won't bother with extra time.
            minLimit = minutes + ((period - 1) * 45)
    where
    events = filter (\e -> F24.period_id e == period) allEvents
    frames = filter isInPeriod allFrames
    isInPeriod f =
        (Tracab.frameId f <= p1end) && (Tracab.frameId f >= p1start)
    p1start = Tracab.startFrame tracabPeriod
    p1end = Tracab.endFrame tracabPeriod
    tracabPeriod = periods !! (period -1)
    periods = Tracab.periods tbMeta

-- The penalty for leaving frames unaligned needs to be small.
-- Conversely, leaving events unaligned should be costly.
-- Note that the score for a Match is negative on the log-density scale.
gapl :: Tracab.Frame -> Double
gapl = \f -> (-10.0)    -- Leaves a frame unaligned for p < exp(-10) = 4.5e-5
gapr :: F24.Event Tracab.Coordinates -> Double
gapr = \e -> (-1000.0)

alignEventsAndFrames :: ScoringFunction -> [F24.Event Tracab.Coordinates] -> [Tracab.Frame] -> NW.Alignment (F24.Event Tracab.Coordinates) Tracab.Frame
alignEventsAndFrames scoring events frames =
    NW.align events frames scoring gapl gapr
    where
