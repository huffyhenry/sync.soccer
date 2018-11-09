import qualified Data.IntMap as Map
import System.Environment (getArgs)
import qualified Tracab
import qualified F24

main :: IO ()
main
 =  do
    (f24Filename : tracabFilename : tracabMetaFilename : clArguments) <- getArgs
    game <- F24.loadGameFromFile f24Filename
    dataLines <- Tracab.parseFile tracabFilename
    tcMeta <- Tracab.parseMetaFile tracabMetaFilename
    putStrLn ("There are " ++ (show $ length (F24.events game)) ++ " events.")
    putStrLn ("There are " ++ (show $ length dataLines) ++ " tracab data lines")
    putStrLn $ show tcMeta
