module Tracab where

import qualified Data.IntMap as Map
import qualified Data.List.Split as Split
import Data.Maybe (maybe, Maybe, listToMaybe)
import System.IO  (openFile, hGetContents, hClose, IOMode(ReadMode))
import System.Environment (getArgs)
import Text.XML.Light.Types ( Element )
import Text.Printf (printf)
import qualified XmlUtils
import XmlUtils ( attrLookupStrict, attrLookup )
import qualified XmlUtils as Xml


-- Complete Tracab data
type Tracab = (Metadata, Frames)

metadata :: Tracab -> Metadata
metadata = fst

frames :: Tracab -> Frames
frames = snd

parseTracab :: String -> String -> IO Tracab
parseTracab metafile datafile = do
    tracabMeta <- parseMetaFile metafile
    tracabData <- parseDataFile tracabMeta datafile
    return (tracabMeta, tracabData)

data Coordinates = Coordinates {
  x :: Int,
  y :: Int
}

-- The position information of a single player/ball in a single snapshot
data Position = Position{
    participantId :: Int,
    coordinates :: Coordinates,
    mTeam :: Maybe TeamKind,
    speed :: Float,
    mBallStatus :: Maybe BallStatus
}
type Positions = Map.IntMap Position

data BallStatus = Alive | Dead
-- A single complete snapshot of tracking data
data Frame = Frame{
    frameId :: Int,
    positions :: Positions,
    ballPosition :: Position,
    clock :: Maybe Double
    }
type Frames = [Frame]

instance Show Frame where
    show f =
        let formatClock :: Double -> String
            formatClock c = printf "%02.d:%02d.%03d" mins secs msec where
                mins = floor (c / 60.0) :: Int
                secs = floor (c - 60.0*(fromIntegral mins)) :: Int
                msec = round (1000.0*(c - 60.0*(fromIntegral mins) - (fromIntegral secs))) :: Int
            base = "Frame " ++ show (frameId f)
            extra = case clock f of
                Nothing -> ""
                Just c -> printf " (implied clock: %s)" (formatClock c)
        in base ++ extra

-- The key method parsing a line of the Tracab data file into a Frame object
parseFrame :: Metadata -> String -> Frame
parseFrame meta inputLine =
  Frame
    { frameId = frameId
    , positions = positions
    , ballPosition = parseBallPosition ballString
    , clock = clock
    }
  where
  -- Split input data into chunks
  [dataLineIdStr, positionsString, ballString, _] = splitOn ':' inputLine
  positionsStrings = splitOn ';' positionsString

  -- Assemble parsed data
  frameId = read dataLineIdStr
  positions = foldl addPosition Map.empty (map parsePosition positionsStrings)
  addPosition mapg posn = Map.insert (participantId posn) posn mapg

  -- Compute the implied timestamp of the frame in seconds from period start
  inPeriodClock p = let offset = frameId - (startFrame p)
                        fps = frameRateFps meta
                    in (fromIntegral offset) / (fromIntegral fps)
  candidatePeriods = [p | p <- periods meta,
                          startFrame p <= frameId,
                          endFrame p >= frameId]
  clock = maybe Nothing (Just . inPeriodClock) (listToMaybe candidatePeriods)

  -- Parse individual chunks
  splitOn c = Split.wordsBy (==c)
  parsePosition inputStr =
      Position
        { participantId = read idStr
        , coordinates = Coordinates { x = read xStr, y = read yStr }
        , mTeam = team
        , speed = read speedStr
        , mBallStatus = Nothing
        }
      where
      [teamStr,idStr,jerseyNumberStr,xStr,yStr,speedStr] = splitOn ',' inputStr
      team =
        case teamStr of
            "1" ->
                Just Home
            "0" ->
                Just Away
            _ ->
                Nothing

  parseBallPosition inputStr =
      Position
        { participantId = 0
        , coordinates = Coordinates { x = read xStr, y = read yStr }
        , mTeam = team
        , mBallStatus = ballStatus
        , speed = read speedStr
        }
      where
      xStr:yStr:zStr:speedStr:rest = (splitOn ',') inputStr
      (team, otherFields) =
        case rest of
            "H" : remainder ->
                (Just Home, remainder)
            "A" : remainder ->
                (Just Away, remainder)
            _ ->
                (Nothing, rest)
      ballStatus =
        case otherFields of
            "Alive" : _ ->
                Just Alive
            "Dead" : _ ->
                Just Dead
            _ ->
                Nothing


-- Parse the entire Tracab data file into a list of frames
parseDataFile :: Metadata -> String -> IO Frames
parseDataFile meta filename =
  do
    handle <- openFile filename ReadMode
    contents <- hGetContents handle
    let frames = map (parseFrame meta) $ lines contents
    return frames


{- An example meta file:

<TracabMetaData sVersion="1.0">
    <match iId="803174" dtDate="2015-08-16 17:00:00" iFrameRateFps="25"
        fPitchXSizeMeters="105.00" fPitchYSizeMeters="68.00"
        fTrackingAreaXSizeMeters="111.00" fTrackingAreaYSizeMeters="88.00">
        <period iId="1" iStartFrame="1349935" iEndFrame="1424747"/>
        <period iId="2" iStartFrame="1449116" iEndFrame="1521187"/>
        <period iId="3" iStartFrame="0" iEndFrame="0"/>
        <period iId="4" iStartFrame="0" iEndFrame="0"/>
    </match>
</TracabMetaData>

-}

-- The type of Tracab metadata
data Metadata = Metadata{
    matchId :: String,
    frameRateFps :: Int,
    pitchSizeX :: Float,
    pitchSizeY :: Float,
    trackingX :: Float,
    trackingY :: Float,
    periods :: Periods
}


data Period = Period {
    periodId :: Int,
    startFrame :: Int,
    endFrame :: Int
}
type Periods = [Period]


indentLines :: [String] -> String
indentLines inputLines =
  unlines $ map ("    " ++) inputLines

indent :: String -> String
indent input =
  indentLines $ lines input

instance Show Metadata where
  show match =
      unlines
        [ "matchId: " ++ (matchId match)
        , "frameRateFps: " ++ show (frameRateFps match)
        , "periods: " ++ (indentLines $ map show (periods match))
        ]

instance Show Period where
    show period =
        unwords
          [ "["
          , show (periodId period)
          , "]"
          , "start:"
          , show $ startFrame period
          , show $ endFrame period
          ]

parseMetaFile :: String -> IO Metadata
parseMetaFile filename = do
    root <- Xml.loadXmlFromFile filename
    return $ makeMetadata (head $ Xml.getAllChildren root)

makeMetadata :: Element -> Metadata
makeMetadata element =
    Metadata
      { matchId = attrLookupStrict element id "iId"
      , frameRateFps = attrLookupStrict element read "iFrameRateFps"
      , pitchSizeX = attrLookupStrict element read "fPitchXSizeMeters"
      , pitchSizeY = attrLookupStrict element read "fPitchYSizeMeters"
      , trackingX = attrLookupStrict element read "fTrackingAreaXSizeMeters"
      , trackingY = attrLookupStrict element read "fTrackingAreaYSizeMeters"
      , periods = map makePeriod $ Xml.getChildrenWithQName "period" element
      }

makePeriod :: Element -> Period
makePeriod element =
    Period{
        periodId = attrLookupStrict element read "iId",
        startFrame = attrLookupStrict element read "iStartFrame",
        endFrame = attrLookupStrict element read "iEndFrame"
    }


data TeamKind = Home | Away deriving (Eq, Show)


oppositionKind :: TeamKind -> TeamKind
oppositionKind Home = Away
oppositionKind Away = Home

rightToLeftFirstHalf :: Frames -> TeamKind
rightToLeftFirstHalf tbData =
    case homeX > awayX of
        True ->
            Home
        False ->
            Away
    where
    -- Might be able to do better than this.
    kickOffFrame = head tbData
    kickOffPositions = Map.elems $ positions kickOffFrame
    homePositions = filter (\p -> mTeam p == Just Home) kickOffPositions
    awayPositions = filter (\p -> mTeam p == Just Away) kickOffPositions

    sumX positions = sum $ map (\p -> x $ coordinates p) positions
    homeX = sumX homePositions
    awayX = sumX awayPositions
