{-# LANGUAGE FlexibleInstances #-}

import qualified Data.IntMap as Map
import Data.Maybe (fromJust)
import System.Environment (getArgs)
import Statistics.Distribution (logDensity)
import Statistics.Distribution.Normal as Gaussian
import qualified Tracab
import qualified F24
import qualified NeedlemanWunsch as NW
import qualified Csvs as CSV


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
totalScore offset e f = (clockScore 1.0 offset e f)


main :: IO ()
main = do
    tbMeta <- Tracab.parseMetaFile "data/tracab/metadata/803174_Man City-Chelsea_metadata.xml"
    tbData <- Tracab.parseDataFile tbMeta "data/tracab/803174_Man City-Chelsea.dat"
    f24Raw <- F24.loadGameFromFile "data/f24/f24-8-2015-803174-eventdetails.xml"
    let flippedFirstHalf = Tracab.rightToLeftFirstHalf tbData
    let f24Data = F24.convertGameCoordinates flippedFirstHalf tbMeta f24Raw
    let events = filter (\e -> F24.period_id e == 1) (F24.events f24Data)
    let p1start = (Tracab.startFrame . head . Tracab.periods) tbMeta
    let p1end = (Tracab.endFrame . head . Tracab.periods) tbMeta
    let frames = filter (\f -> (Tracab.frameId f <= p1end) && (Tracab.frameId f >= p1start)) tbData

    -- Take just the first ~20 minutes to stay under 16GB RAM for now.
    let events2 = filter (\e -> F24.min e < 20) events
    let frames2 = take (25*60*25) frames

    -- So if you want to do some smoothing using matrices for the frame data then
    let frameMatrices = Tracab.translateFrames frames2

    -- The penalty for leaving frames unaligned needs to be small.
    -- Conversely, leaving events unaligned should be costly.
    -- Note that the score for a Match can be negative on the log-density scale
    let gapl = -10.0    -- Leaves a frame unaligned for P < exp(-10) = 4.5e-5
    let gapr = -1000.0
    let sim = totalScore 0.0
    let sync = NW.align events2 frames2 sim gapl gapr
    putStrLn $ show sync
    putStr $ (show $ length frames2) ++ " frames, "
    putStr $ (show $ length events2) ++ " events, "
    putStrLn $ "total score " ++ show (NW.alignmentScore sim gapl gapr sync) ++ "."

    -- Write parsed data to CSVs for animation
    CSV.frames2Csv frames2 "data/csv/frames.csv"
    CSV.events2Csv events2 "data/csv/events.csv"
    CSV.alignment2Csv sync "data/csv/sync.csv"
