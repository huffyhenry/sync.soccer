import qualified Data.IntMap as Map
import System.Environment (getArgs)
import qualified Tracab
import qualified F24
import qualified NeedlemanWunsch as Nw

main :: IO ()
main
 =  do
     let gap = -1.0
     let sim = \x -> \y -> if x==y then 1.0 else -1.0
     putStr $ Nw.showAlignment $ Nw.align "GATTACA" "GCATGCU" sim gap
