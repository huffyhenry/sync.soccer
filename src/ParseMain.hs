import qualified Data.IntMap as Map
import System.Environment (getArgs)
import qualified Tracab as Tcb
import qualified F24

main :: IO ()
main =
   do
      (f24MetaFilename : f24Filename : tracabMetaFilename : tracabDataFilename : clArguments) <- getArgs
      f24Meta <- F24.parseMetaFile f24MetaFilename
      game <- F24.loadGameFromFile f24Filename
      putStrLn $ displayShirtNumbers f24Meta

      (tcMeta, tcData) <- Tcb.parseTracab tracabMetaFilename tracabDataFilename
      putStrLn ("There are " ++ (show $ length (F24.events game)) ++ " events.")
      putStrLn ("There are " ++ (show $ length tcData) ++ " tracab data lines")
      -- This tests the parser by forcing us to evaluate the entirety of each frame.
      let maxFrameInt = maximum $ map Tcb.frameInteger tcData
      putStrLn ("The maximum frame int (for parser testing) is: " ++ show maxFrameInt)
      putStrLn $ show tcMeta
      -- This tests the parser by forcing us to evaluate the entirety of each frame.
      let maxFrameInt = maximum $ map Tcb.frameInteger tcData
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
