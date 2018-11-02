import qualified Data.IntMap as Map
import System.Environment (getArgs)
import qualified Tracab
import qualified F24

main :: IO ()
main
 =  do
    (f24Filename : tracabFilename : clArguments) <- getArgs
    game <- F24.loadGameFromFile f24Filename
    dataLines <- Tracab.parseFile tracabFilename
    putStrLn ("There are " ++ (show $ length dataLines) ++ " tracab data lines")
    putStrLn ("There are " ++ (show $ length (F24.events game)) ++ " events.")

