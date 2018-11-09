module Tracab where

import qualified Data.IntMap as Map
import System.IO  (openFile, hGetContents, hClose, IOMode(ReadMode))
import System.Environment (getArgs)
import qualified Data.List.Split as Split
import qualified Data.Maybe as Maybe
import Text.XML.Light.Types ( Element )
import qualified XmlUtils
import XmlUtils ( attrLookupStrict, attrLookup )
import qualified XmlUtils as Xml


data Position = Position
    { participantId :: Int
    , x :: Int
    , y :: Int
    , teamId :: Int
    , speed :: Float
    }

type ShirtNumber = Int
type Positions = Map.IntMap Position
type DataLineId = Int
data DataLine = DataLine
    { dataLineId :: DataLineId
    , positions :: Positions
    , ballPosition :: Position
    }


parseLine :: String -> DataLine
parseLine inputLine =
  DataLine
    { dataLineId = dataLineId
    , positions = positions
    , ballPosition = parseBallPosition ballString
    }
  where
  dataLineId =
      read dataLineIdStr

  positions =
    foldl addPosition Map.empty positionsList

  addPosition mapping position =
    Map.insert (participantId position) position mapping

  positionsStrings = Split.wordsBy (==';') positionsString
  positionsList =
      map parsePosition positionsStrings
  parsePosition inputStr =
      Position
        { participantId = read idStr
        , x = read xStr
        , y = read yStr
        , teamId = read teamStr
        , speed = read speedStr
        }
      where
      [teamStr,idStr,jerseyNumberStr,xStr,yStr,speedStr] = commaSplit inputStr
  parseBallPosition inputStr =
      Position
        { participantId = 0
        , x = read xStr
        , y = read yStr
        , teamId = read teamStr
        , speed = read speedStr
        }
        where
        [xStr, yStr, speedStr, teamStr, statusStr, _] = commaSplit inputStr
  [dataLineIdStr, positionsString, ballString, _] = colonSplit inputLine

commaSplit :: String -> [String]
commaSplit = splitOn ','

colonSplit :: String -> [String]
colonSplit = splitOn ':'

splitOn :: Char -> String -> [ String ]
splitOn c = Split.wordsBy (== c)

parseFile :: String ->  IO [ DataLine ]
parseFile filename =
  do
    handle <- openFile filename ReadMode
    contents <- hGetContents handle
    let dataLines = map parseLine $ lines contents
    -- hClose handle
    return dataLines


maxSpeed :: [ DataLine ] -> Float
maxSpeed dataLines =
  maximum $ map getMaxSpeed dataLines
  where
  getMaxSpeed line =
    maximum $ map speed $ Map.elems (positions line)

main :: IO ()
main = do
  (filename : clArguments) <- getArgs
  dataLines <- parseFile filename
  putStr $ show $ maxSpeed dataLines



{- An example meta file:

<TracabMetaData sVersion="1.0">
    <match iId="803174" dtDate="2015-08-16 17:00:00" iFrameRateFps="25" fPitchXSizeMeters="105.00" fPitchYSizeMeters="68.00" fTrackingAreaXSizeMeters="111.00" fTrackingAreaYSizeMeters="88.00">
        <period iId="1" iStartFrame="1349935" iEndFrame="1424747"/>
        <period iId="2" iStartFrame="1449116" iEndFrame="1521187"/>
        <period iId="3" iStartFrame="0" iEndFrame="0"/>
        <period iId="4" iStartFrame="0" iEndFrame="0"/>
    </match>
</TracabMetaData>


-}

data Match = Match {
    matchId :: String
  , frameRateFps :: Int
  , pitchSizeX :: Float
  , pitchSizeY :: Float
  , trackingX :: Float
  , trackingY :: Float
  , periods :: [ Period ]
}


data Period = Period {
    periodId :: Int
  , startFrame :: Int
  , endFrame :: Int
  }


indentLines :: [ String ] -> String
indentLines inputLines =
  unlines $ map ("    " ++) inputLines

indent :: String -> String
indent input =
  indentLines $ lines input

instance Show Match where
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

parseMetaFile :: String -> IO Match
parseMetaFile filename = do
    root <- Xml.loadXmlFromFile filename
    return $ makeMatch (head $ Xml.getAllChildren root)

makeMatch :: Element -> Match
makeMatch element =
    Match
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
    Period
      { periodId = attrLookupStrict element read "iId"
      , startFrame = attrLookupStrict element read "iStartFrame"
      , endFrame = attrLookupStrict element read "iEndFrame"
      }
