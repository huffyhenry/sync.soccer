module Scoring where

import qualified Data.Map.Strict as Map
import qualified Data.List
import qualified Tracab
import qualified F24

eventPlayerDistance :: F24.ShirtNumbers -> F24.Event Tracab.Coordinates -> Tracab.Frame Tracab.Positions -> Maybe Double
eventPlayerDistance shirtNumbers event frame =
    do
        playerId <- F24.player_id event
        (teamKind, shirtNumber) <- Map.lookup playerId shirtNumbers
        let isPlayer p = Tracab.participantId p == shirtNumber && Tracab.mTeam p == Just teamKind
        let positions = Tracab.positions frame
        playerPosition <- Data.List.find isPlayer $ Tracab.agents positions
        let playerCoords = Tracab.coordinates playerPosition
        let ballCoords = Tracab.coordinates $ Tracab.ball positions
        return $ euclideanDistance playerCoords ballCoords

euclideanDistance :: Tracab.Coordinates -> Tracab.Coordinates -> Double
euclideanDistance object target =
    sqrt $ fromIntegral sumSquareSides
    where
    sumSquareSides = xSquareSide + ySquareSide
    xSquareSide = squareSide (Tracab.x object) (Tracab.x target)
    ySquareSide = squareSide (Tracab.y object) (Tracab.y target)

    squareSide p b = (p - b) ^ 2
