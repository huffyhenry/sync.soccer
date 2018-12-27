module Scoring where

import qualified Data.List
import qualified Tracab
import qualified F24

eventPlayerDistance :: F24.Event Tracab.Coordinates -> Tracab.Frame Tracab.Positions -> Maybe Double
eventPlayerDistance event frame =
    case F24.player_id event of
        Nothing ->
            Nothing
        Just playerId ->
            let
                playerPosition = Data.List.find isPlayer (Tracab.agents $ Tracab.positions frame)
                isPlayer p =
                    Tracab.participantId p == playerId
            in
            case playerPosition of
                Nothing ->
                    Nothing
                Just position ->
                    Just distance
                    where
                    distance = sqrt (fromIntegral $ (squareSide playerX ballX) + (squareSide playerY ballY))
                    squareSide p b = (p - b) ^ 2
                    playerX = Tracab.x $ Tracab.coordinates position
                    playerY = Tracab.y $ Tracab.coordinates position
                    ballCoords = Tracab.coordinates $ Tracab.ball $ Tracab.positions frame
                    ballX = Tracab.x ballCoords
                    ballY = Tracab.y ballCoords
