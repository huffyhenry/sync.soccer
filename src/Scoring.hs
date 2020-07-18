module Scoring where

import qualified Data.Map.Strict as Map
import qualified Data.List
import Data.Maybe (maybe, fromMaybe)
import qualified Tracab as Tcb
import qualified F24

-- Distance between the player performing the given event and the ball,
-- assessed using the Tracab frame supplied.
playerBallDistance :: F24.ShirtNumbers -> F24.Event Tcb.Coordinates -> Tcb.Frame Tcb.Positions -> Maybe Double
playerBallDistance shirtNumbers event frame = do
    playerId <- F24.player_id event
    (teamKind, shirtNumber) <- Map.lookup playerId shirtNumbers
    let isPlayer p = Tcb.shirtNumber p == Just shirtNumber && Tcb.mTeam p == Just teamKind
    let positions = Tcb.positions frame
    playerPosition <- Data.List.find isPlayer $ Tcb.agents positions
    let playerCoords = Tcb.coordinates playerPosition
    let ballCoords = Tcb.coordinates $ Tcb.ball positions
    return $ eucl playerCoords ballCoords

-- Euclidean distance between a pair of points.
eucl :: Tcb.Coordinates -> Tcb.Coordinates -> Double
eucl object target = sqrt $ fromIntegral (xSquareSide + ySquareSide) where    
    xSquareSide = squareSide (Tcb.x object) (Tcb.x target)
    ySquareSide = squareSide (Tcb.y object) (Tcb.y target)
    squareSide p b = (p - b) ^ 2

-- Functions that measure (in)compatibility between an event and a frame.
-- Raw measurements are normalised using the first arguments (should be +ve.)
type Mismatch = F24.Event Tcb.Coordinates -> Tcb.Frame Tcb.Positions -> Double
type RawMismatch = Double -> Mismatch

-- Mismatch between clocks.
misClock :: Bool -> F24.Game Tcb.Coordinates -> RawMismatch
misClock useTimestamp game scale e f = dist / scale where
    seconds = F24.eventClock useTimestamp game e
    dist = abs $ seconds - fromMaybe seconds (Tcb.clock f)

-- Mismatch between event location and ball position.
misLocation :: RawMismatch
misLocation scale e f = dist / scale where
    eXY = fromMaybe fXY (F24.coordinates e)
    fXY = Tcb.coordinates $ Tcb.ball $ Tcb.positions f
    dist = eucl eXY fXY

-- Disagreement on ball status (all events imply that the ball is in play.)
misBallStatus :: RawMismatch
misBallStatus scale _ f = if status == Just Tcb.Dead then scale else 0 where 
    status = Tcb.mBallStatus $ Tcb.ball $ Tcb.positions f 

-- Distance between the player and the ball (all events imply 0.)
misPlayer :: F24.ShirtNumbers -> RawMismatch
misPlayer jerseys scale e f = dist / scale where
    dist = fromMaybe 0.0 (playerBallDistance jerseys e f)

-- Combine an arbitrary number of Mismatch functions to a single one
-- by taking the length of the vector of individual mismatch scores.
combine :: [Mismatch] -> Mismatch
combine funs e f = sqrt $ sum [fun e f ^ 2  | fun <- funs]