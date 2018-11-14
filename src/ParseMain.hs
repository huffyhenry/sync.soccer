import qualified Data.IntMap as Map
import System.Environment (getArgs)
import qualified Tracab
import qualified F24

main :: IO ()
main
 =  do
    (f24Filename : tracabMetaFilename : tracabDataFilename : clArguments) <- getArgs
    game <- F24.loadGameFromFile f24Filename
    (tcMeta, tcData) <- Tracab.parseTracab tracabMetaFilename tracabDataFilename
    putStrLn ("There are " ++ (show $ length (F24.events game)) ++ " events.")
    putStrLn ("There are " ++ (show $ length tcData) ++ " tracab data lines")
    putStrLn $ show tcMeta
