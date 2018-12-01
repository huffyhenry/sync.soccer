{-# LANGUAGE FlexibleInstances #-}

import qualified Data.IntMap as Map
import Data.Maybe (fromJust)
import System.Environment (getArgs)
import qualified Tracab
import qualified F24
import qualified NeedlemanWunsch as NW

-- Make String-String and F24-Tracab alignments printable
instance Show (NW.Alignment Char Char) where
    show (NW.Alignment []) = []
    show (NW.Alignment ((NW.GapL y):ps)) = "|" ++ [y] ++ "\n" ++ show (NW.Alignment ps)
    show (NW.Alignment ((NW.GapR x):ps)) = [x] ++ "|" ++ "\n" ++ show (NW.Alignment ps)
    show (NW.Alignment ((NW.Match x y):ps)) = [x] ++ [y] ++ "\n" ++ show (NW.Alignment ps)

instance Show (NW.Alignment (F24.Event a) Tracab.Frame) where
    show (NW.Alignment []) = ""
    show (NW.Alignment ((NW.Match e f):rest)) =
        let
            padRight n s = foldl (++) "" (replicate (n - length s) " ")
            getTime = \e -> (show . F24.min) e ++ ":" ++ (show . F24.sec) e
            getClock = \f -> show $ ((fromJust . Tracab.clock) f) / 60.0
            eDesc =  getTime e ++ " " ++ F24.eventTypeName e
            fDesc = "Frame " ++ (show . Tracab.frameId) f ++ " (clock=" ++ getClock f ++ ")"
        in eDesc ++ (padRight 30 eDesc) ++ fDesc ++ "\n" ++ show (NW.Alignment rest)
    show (NW.Alignment (_:rest)) = ".\n" ++ show (NW.Alignment rest)


-- A toy dissimilarity function of Events and Frames
sim :: F24.Event Tracab.Coordinates -> Tracab.Frame -> Double
sim e f = timePenalty + locationPenalty where
    timePenalty = abs $ (seconds e) - (fromJust $ Tracab.clock f)
    locationPenalty = distance e f

    seconds x = fromIntegral $ 60 * (F24.min x) + (F24.sec x)
    distance ev fr = let
        evX = (Tracab.x . fromJust . F24.coordinates) ev
        frX = (Tracab.x . Tracab.coordinates . Tracab.ballPosition) fr
        in fromIntegral $ abs $ evX-frX

main :: IO ()
main = do
    tbMeta <- Tracab.parseMetaFile "data/tracab/metadata/803174_Man City-Chelsea_metadata.xml"
    tbData <- Tracab.parseDataFile tbMeta "data/tracab/803174_Man City-Chelsea.dat"
    f24Raw <- F24.loadGameFromFile "data/f24/f24-8-2015-803174-eventdetails.xml"
    let f24Data = F24.convertCoordinates tbMeta f24Raw
    let events = filter (\e -> F24.period_id e == 1) (F24.events f24Data)
    let p1start = (Tracab.startFrame . head . Tracab.periods) tbMeta
    let p1end = (Tracab.endFrame . head . Tracab.periods) tbMeta
    let frames = filter (\f -> (Tracab.frameId f <= p1end) && (Tracab.frameId f >= p1start)) tbData
    let gap = 1.0
    let sync = NW.align events frames sim gap
    putStrLn $ show sync
