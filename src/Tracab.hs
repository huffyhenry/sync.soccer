module Tracab where

import qualified Data.IntMap as Map
import qualified Data.List.Split as Split
import System.IO  (openFile, hGetContents, hClose, IOMode(ReadMode))
import System.Environment (getArgs)
import Text.XML.Light.Types ( Element )
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
    tracabData <- parseDataFile datafile
    return (tracabMeta, tracabData)

-- The position information of a single player/ball in a single snapshot
data Position = Position{
    participantId :: Int,
    x :: Int,
    y :: Int,
    teamId :: Int,
    speed :: Float
}
type Positions = Map.IntMap Position

-- A single complete snapshot of tracking data
data Frame = Frame{
    frameId :: Int,
    positions :: Positions,
    ballPosition :: Position
    }
type Frames = [Frame]

-- The key method parsing a line of the Tracab data file into a Frame object
parseFrame :: String -> Frame
parseFrame inputLine =
  Frame
    { frameId = frameId
    , positions = positions
    , ballPosition = parseBallPosition ballString
    }
  where
  -- Split input data into chunks
  [dataLineIdStr, positionsString, ballString, _] = splitOn ':' inputLine
  positionsStrings = splitOn ';' positionsString

  -- Assemble parsed data
  frameId = read dataLineIdStr
  positions = foldl addPosition Map.empty (map parsePosition positionsStrings)
  addPosition mapg posn = Map.insert (participantId posn) posn mapg

  -- Parse individual chunks
  splitOn c = Split.wordsBy (==c)
  parsePosition inputStr =
      Position
        { participantId = read idStr
        , x = read xStr
        , y = read yStr
        , teamId = read teamStr
        , speed = read speedStr
        }
      where
      [teamStr,idStr,jerseyNumberStr,xStr,yStr,speedStr] = splitOn ',' inputStr
  parseBallPosition inputStr =
      Position
        { participantId = 0
        , x = read xStr
        , y = read yStr
        , teamId = read teamStr
        , speed = read speedStr
        }
      where
      [xStr, yStr, speedStr, teamStr, statusStr, _] = splitOn ',' inputStr


-- Parse the entire Tracab data file into a list of frames
parseDataFile :: String -> IO Frames
parseDataFile filename =
  do
    handle <- openFile filename ReadMode
    contents <- hGetContents handle
    let frames = map parseFrame $ lines contents
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
      , trackingY = attrLookupStrict element read "fTrakcingAreaYSizeMeters"
      , periods = map makePeriod $ Xml.getChildrenWithQName "period" element
      }

makePeriod :: Element -> Period
makePeriod element =
    Period{
        periodId = attrLookupStrict element read "iId",
        startFrame = attrLookupStrict element read "iStartFrame",
        endFrame = attrLookupStrict element read "iEndFrame"
    }
