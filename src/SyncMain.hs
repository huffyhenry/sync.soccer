import qualified Data.IntMap as Map
import Data.Maybe (fromJust)
import System.Environment (getArgs)
import qualified Tracab
import qualified F24
import qualified NeedlemanWunsch as Nw

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
    let flippedFirstHalf = Tracab.rightToLeftFirstHalf tbData
    let f24Data = F24.convertGameCoordinates flippedFirstHalf tbMeta f24Raw
    let events = filter (\e -> F24.period_id e == 1) (F24.events f24Data)
    let p1start = (Tracab.startFrame . head . Tracab.periods) tbMeta
    let p1end = (Tracab.endFrame . head . Tracab.periods) tbMeta
    let frames = filter (\f -> (Tracab.frameId f <= p1end) && (Tracab.frameId f >= p1start)) tbData
    let gap = 1.0
    let sync = Nw.align events frames sim gap
    putStrLn $ show flippedFirstHalf
    putStrLn $ show $ length frames
    putStrLn $ show $ length events
    putStrLn $ show $ length sync
