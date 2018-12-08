{-# LANGUAGE FlexibleInstances #-}

module Csvs where

import Data.IntMap (elems)
import Data.Maybe (fromJust)
import Text.Printf (printf)
import qualified Tracab as Tcb
import qualified F24 as F24
import qualified NeedlemanWunsch as NW


events2Csv :: [F24.Event (Tcb.Coordinates)] -> String -> IO ()
events2Csv events filepath = do
    let header = "event,x,y,type,team,min,sec"
    let format = "%d,%d,%d,%s,%d,%d,%d"
    let e2record e = printf format (F24.eid e)
                                   ((Tcb.x . fromJust . F24.coordinates) e)
                                   ((Tcb.x . fromJust . F24.coordinates) e)
                                   (F24.eventTypeName e)
                                   (F24.team_id e)
                                   (F24.min e)
                                   (F24.sec e)
    writeFile filepath (unlines (header:(map e2record events)))


frames2Csv :: Tcb.Frames -> String -> IO ()
frames2Csv frames filepath = do
    let header = "x,y,object,team,clock,frame"
    let format = "%d,%d,%d,%d,%.3f,%d"
    let fp2record (f, p) = printf format (Tcb.x (Tcb.coordinates p))
                                         (Tcb.y (Tcb.coordinates p))
                                         (Tcb.participantId p)
                                         (Tcb.teamId p)
                                         (fromJust (Tcb.clock f))
                                         (Tcb.frameId f)
    let meltFrame f = (f, Tcb.ballPosition f):[(f, pos) | pos <- elems (Tcb.positions f)]
    let melted = foldr (++) [] (map meltFrame frames)
    let records = map fp2record melted
    writeFile filepath (unlines (header:records))


alignment2Csv :: NW.Alignment (F24.Event Tcb.Coordinates) Tcb.Frame -> String -> IO ()
alignment2Csv (NW.Alignment pairs) filepath = do
    let header = "event,frame"
    let isMatch (NW.Match _ _) = True
        isMatch _ = False
    let match2record (NW.Match e f) = printf "%d,%d" (F24.eid e) (Tcb.frameId f)
    let records = map match2record (filter isMatch pairs)
    writeFile filepath (unlines (header:records))

