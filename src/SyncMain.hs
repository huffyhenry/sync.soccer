{-# LANGUAGE FlexibleInstances #-}

import qualified Data.IntMap as Map
import Data.Maybe (fromJust)
import System.Environment (getArgs)
import Statistics.Distribution (logDensity)
import Statistics.Distribution.Normal as Gaussian
import qualified Tracab
import qualified F24
import qualified NeedlemanWunsch as NW


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
totalScore offset e f = (clockScore 1.0 offset e f) + (locationScore 1.0 e f)


main :: IO ()
main = do
    tbMeta <- Tracab.parseMetaFile "data/tracab/metadata/803174_Man City-Chelsea_metadata.xml"
    tbData <- Tracab.parseDataFile tbMeta "data/tracab/803174_Man City-Chelsea.dat"
    f24Raw <- F24.loadGameFromFile "data/f24/f24-8-2015-803174-eventdetails.xml"
    let flippedFirstHalf = Tracab.rightToLeftFirstHalf tbData
    let f24Data = F24.convertCoordinates flippedFirstHalf tbMeta f24Raw
    let events = filter (\e -> F24.period_id e == 1) (F24.events f24Data)
    let p1start = (Tracab.startFrame . head . Tracab.periods) tbMeta
    let p1end = (Tracab.endFrame . head . Tracab.periods) tbMeta
    let frames = filter (\f -> (Tracab.frameId f <= p1end) && (Tracab.frameId f >= p1start)) tbData
    let sync = NW.align events frames (totalScore 0.0) (-1000000.0) 0.0
    putStrLn $ show sync
