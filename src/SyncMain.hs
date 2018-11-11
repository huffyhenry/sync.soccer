import qualified Data.IntMap as Map
import System.Environment (getArgs)
import qualified Tracab
import qualified F24
import qualified NeedlemanWunsch as NW

printAlignment :: NW.Alignment Char Char -> String
printAlignment [] = []
printAlignment ((NW.GapL y):ps) = "|" ++ [y] ++ "\n" ++ printAlignment ps
printAlignment ((NW.GapR x):ps) = [x] ++ "|" ++ "\n" ++ printAlignment ps
printAlignment ((NW.Match x y):ps) = [x] ++ [y] ++ "\n" ++ printAlignment ps

main :: IO ()
main
 =  do
     let gap = -1.0
     let sim = \x -> \y -> if x==y then 1.0 else -1.0
     putStr $ printAlignment $ NW.nw "GATTACA" "GCATGCU" sim gap
