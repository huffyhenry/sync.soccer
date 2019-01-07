import qualified Data.IntMap as Map
import System.Environment (getArgs)
import qualified F24
import Tracab

main :: IO ()
main =
   do
      (f24MetaFilename : f24Filename : tracabMetaFilename : tracabDataFilename : clArguments) <- getArgs
      f24Meta <- F24.parseMetaFile f24MetaFilename
      game <- F24.loadGameFromFile f24Filename
      putStrLn $ displayShirtNumbers f24Meta

      (tcMeta, tcData) <- parseTracab tracabMetaFilename tracabDataFilename
      putStrLn ("There are " ++ (show $ length (F24.events game)) ++ " events.")
      putStrLn ("There are " ++ (show $ length tcData) ++ " tracab data lines")
      -- This tests the parser by forcing us to evaluate the entirety of each frame.
      let maxFrameInt = maximum $ map frameInteger tcData
      putStrLn ("The maximum frame int (for parser testing) is: " ++ show maxFrameInt)
      putStrLn $ show tcMeta
      -- This tests the parser by forcing us to evaluate the entirety of each frame.
      let maxFrameInt = maximum $ map frameInteger tcData
      putStrLn ("The maximum frame int (for parser testing) is: " ++ show maxFrameInt)


displayShirtNumbers :: F24.Metadata -> String
displayShirtNumbers meta =
   unlines allLines
   where
   allLines = homeLines ++ awayLines
   homeLines = map (displayShirt "Home:") $ F24.players $ F24.homeTeam meta
   awayLines = map (displayShirt "Away:") $ F24.players $ F24.awayTeam meta
   displayShirt team player =
      unwords
         [ team
         , F24.playerRef player
         , "wears number"
         , show $ F24.shirtNumber player
         ]

{-
    This is really just a dummy function to convert the entirety of a parsed frame,
    including the positions, into a single integer. The trick here is to make sure that
    we utilise every single part of the frame, this forces evaluation of the parsed frame
    and hence any parser errors to come to light.
-}
frameInteger :: Frame Positions -> Int
frameInteger frame =
    sum
        [ frameId frame
        , clockInt
        , sum $ map positionInt (agents framePositions)
        , positionInt $ ball framePositions
        ]
    where
    framePositions = positions frame
    clockInt = case clock frame of
                  Nothing -> 0
                  Just x  -> round x
    positionInt :: Position -> Int
    positionInt position =
        sum
            [ participantId position
            , x $ coordinates position
            , y $ coordinates position
            , round $ speed position
            , teamInt
            , ballInt
            ]
        where
        teamInt = case mTeam position of
                     Nothing   -> 0
                     Just Home -> 1
                     Just Away -> 2
        ballInt = case mBallStatus position of
                     Nothing    -> 0
                     Just Alive -> 1
                     Just Dead  -> 2
