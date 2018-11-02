module Tracab where

import qualified Data.IntMap as Map
import System.IO  (openFile, hGetContents, hClose, IOMode(ReadMode))
import System.Environment (getArgs)
import qualified Data.List.Split as Split
import qualified Data.Maybe as Maybe


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
