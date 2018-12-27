module Scoring where

import qualified Data.List
import qualified Tracab
import qualified F24

eventPlayerDistance :: F24.Event Tracab.Coordinates -> Tracab.Frame Tracab.Positions -> Maybe Double
eventPlayerDistance event frame =
    do
        playerId <- F24.player_id event
        let isPlayer p = Tracab.participantId p == playerId
        playerPosition <- Data.List.find isPlayer (Tracab.agents $ Tracab.positions frame)
        return $ euclideanDistance playerPosition
    where
    euclideanDistance playerPosition =
        sqrt (fromIntegral $ (squareSide playerX ballX) + (squareSide playerY ballY))
        where
        squareSide p b = (p - b) ^ 2
        ballX = Tracab.x ballCoords
        ballY = Tracab.y ballCoords
        playerX = Tracab.x $ Tracab.coordinates playerPosition
        playerY = Tracab.y $ Tracab.coordinates playerPosition
        ballCoords = Tracab.coordinates $ Tracab.ball $ Tracab.positions frame
