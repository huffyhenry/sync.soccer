module Scoring where

import qualified Data.Map.Strict as Map
import qualified Data.List
import qualified Tracab as Tcb
import qualified F24

eventPlayerDistance :: F24.ShirtNumbers -> F24.Event Tcb.Coordinates -> Tcb.Frame Tcb.Positions -> Maybe Double
eventPlayerDistance shirtNumbers event frame =
    do
        playerId <- F24.player_id event
        (teamKind, shirtNumber) <- Map.lookup playerId shirtNumbers
        let isPlayer p = Tcb.participantId p == shirtNumber && Tcb.mTeam p == Just teamKind
        let positions = Tcb.positions frame
        playerPosition <- Data.List.find isPlayer $ Tcb.agents positions
        let playerCoords = Tcb.coordinates playerPosition
        let ballCoords = Tcb.coordinates $ Tcb.ball positions
        return $ euclideanDistance playerCoords ballCoords

euclideanDistance :: Tcb.Coordinates -> Tcb.Coordinates -> Double
euclideanDistance object target =
    sqrt $ fromIntegral sumSquareSides
    where
    sumSquareSides = xSquareSide + ySquareSide
    xSquareSide = squareSide (Tcb.x object) (Tcb.x target)
    ySquareSide = squareSide (Tcb.y object) (Tcb.y target)

    squareSide p b = (p - b) ^ 2
