{-# LANGUAGE FlexibleInstances #-}

import Control.Parallel (par, pseq)
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

clockScore :: Double -> Double -> F24.Event Tracab.Coordinates -> Tracab.Frame Tracab.Positions -> Double
clockScore scale offset e f =
    let seconds = fromIntegral $ 60 * (F24.min e) + (F24.sec e)
        dist = abs $ seconds - (fromJust $ Tracab.clock f)
    in logDensity Gaussian.standard ((dist - offset) / scale)

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

totalScore :: Double -> F24.Event Tracab.Coordinates -> Tracab.Frame Tracab.Positions -> Double
totalScore offset e f = (clockScore 1.0 offset e f) + (locationScore 100.0 e f)

type ScoringFunction = F24.Event Tracab.Coordinates -> Tracab.Frame Tracab.Positions -> Double

-- Command line parsing machinery
data Options = Options {
    tcbMetaFile :: String,
    tcbDataFile :: String,
    f24File     :: String,
    outputFile  :: String
} deriving Show

options :: Parser Options
options = Options
       <$> argument str (metavar "TRACAB-META")
       <*> argument str (metavar "TRACAB-DATA")
       <*> argument str (metavar "F24")
       <*> argument str (metavar "OUTPUT")

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
    let allEvents = F24.events f24Data
    let allFrames = tbData
    let (firstHalfEvents, firstHalfFrames) = getEventsAndFrames 1 (Just 5) tbMeta allEvents allFrames
    let (secondHalfEvents, secondHalfFrames) = getEventsAndFrames 2 (Just 5) tbMeta allEvents allFrames
    -- adc: Marek, you took this option out: https://github.com/huffyhenry/sync.soccer/commit/20e0cc4bd34a1676361d0ccdca768b714f0c92b7#diff-3189e0ce10cae1a44417d5a43cb53d58
    -- is that a permanent thing?
    -- let scoring = if timeOnly opts then clockScore 1.0 0.0 else totalScore 0.0
    let scoring = clockScore 1.0 0.0

    let sync1 = alignEventsAndFrames scoring firstHalfEvents firstHalfFrames
    let sync2 = alignEventsAndFrames scoring secondHalfEvents secondHalfFrames
    -- This stackoverflow question explains why we need do `a par b pseq a+b`.
    -- https://stackoverflow.com/questions/4576734/why-do-we-need-seq-or-pseq-with-par-in-haskell
    -- Note: I'm not sure if this is actually enough, we may need to 'force' the evaluation of a list
    -- see: http://book.realworldhaskell.org/read/concurrent-and-multicore-programming.html
    -- in particular the 'force' function introduced for the parallelSort `parSort` function.
    -- However, note that we might be better to use a 'Strategy' (see later in the same page).
    -- In particular it might be that computing the 'NW.alignmentScore' for each of the two halves
    -- forces the execution of it anyway, so we could do something simple like:
    -- let score1 = NW.alignmentScore scoring gapl gapr sync1
    -- let score2 = NW.alignmentScore scoring gapl gapr sync2
    -- let overallScore = score1 `par` score2 `pseq` score1 + score2
    let sync = sync1 `par` (sync2 `pseq` NW.joinAlignments sync1 sync2)

    putStrLn $ show sync
    putStr $ (show $ length firstHalfEvents) ++ " frames, "
    putStr $ (show $ length firstHalfFrames) ++ " events, "
    putStrLn $ "total score " ++ show (NW.alignmentScore scoring gapl gapr sync) ++ "."

    -- Write parsed data to CSVs for animation
    CSV.frames2Csv firstHalfFrames "data/csv/frames.csv"
    CSV.events2Csv firstHalfEvents "data/csv/events.csv"
    CSV.alignment2Csv sync (outputFile opts)


getEventsAndFrames :: Int -> Maybe Int -> Tracab.Metadata -> [F24.Event Tracab.Coordinates] -> [Tracab.Frame Tracab.Positions ] -> ([F24.Event Tracab.Coordinates], [Tracab.Frame Tracab.Positions])
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
        (Tracab.frameId f <= periodEnd) && (Tracab.frameId f >= periodStart)
    periodStart = Tracab.startFrame tracabPeriod
    periodEnd = Tracab.endFrame tracabPeriod
    tracabPeriod = periods !! (period -1)
    periods = Tracab.periods tbMeta

-- The penalty for leaving frames unaligned needs to be small.
-- Conversely, leaving events unaligned should be costly.
-- Note that the score for a Match is negative on the log-density scale.
gapl :: Tracab.Frame Tracab.Positions -> Double
gapl = \f -> (-10.0)    -- Leaves a frame unaligned for p < exp(-10) = 4.5e-5
gapr :: F24.Event Tracab.Coordinates -> Double
gapr = \e -> (-1000.0)

alignEventsAndFrames :: ScoringFunction -> [F24.Event Tracab.Coordinates] -> [Tracab.Frame Tracab.Positions] -> NW.Alignment (F24.Event Tracab.Coordinates) (Tracab.Frame Tracab.Positions)
alignEventsAndFrames scoring events frames =
    NW.align events frames scoring gapl gapr
    where
    -- So if you want to do some smoothing using matrices for the frame data then
    frameMatrices = Tracab.translateFrames frames
    -- If you want just a list of matrices then
    tracabMatrices = map Tracab.positions frameMatrices
