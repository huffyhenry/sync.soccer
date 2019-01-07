module Scoring where

import qualified Data.Map.Strict as Map
import qualified Data.List
import Data.Maybe (maybe)
import Statistics.Distribution (logDensity)
import Statistics.Distribution.Normal as Gaussian
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
    sqrt $ fromIntegral (xSquareSide + ySquareSide)
    where
    xSquareSide = squareSide (Tcb.x object) (Tcb.x target)
    ySquareSide = squareSide (Tcb.y object) (Tcb.y target)
    squareSide p b = (p - b) ^ 2

clockScore :: Double -> F24.Event Tcb.Coordinates -> Tcb.Frame Tcb.Positions -> Double
clockScore scale e f =
    let seconds = fromIntegral $ 60 * (F24.min e) + (F24.sec e)
        dist = abs $ seconds - (maybe seconds id (Tcb.clock f))
    in logDensity Gaussian.standard (dist / scale)

locationScore :: Double -> F24.Event Tcb.Coordinates -> Tcb.Frame Tcb.Positions -> Double
locationScore scale e f =
    let eXY = maybe fXY id (F24.coordinates e)
        fXY = Tcb.coordinates $ Tcb.ball $ Tcb.positions f
        dist = euclideanDistance eXY fXY
    in logDensity Gaussian.standard (dist / scale)

ballStatusScore :: Double -> F24.Event Tcb.Coordinates -> Tcb.Frame Tcb.Positions -> Double
ballStatusScore scale _ f = case Tcb.mBallStatus $ Tcb.ball $ Tcb.positions f of
                                 Nothing -> 0.0
                                 Just Tcb.Alive -> scale
                                 Just Tcb.Dead -> (-scale)

totalScore :: F24.Event Tcb.Coordinates -> Tcb.Frame Tcb.Positions -> Double
totalScore e f = (clockScore 1.0 e f) + (locationScore 100.0 e f) + (ballStatusScore 1.0 e f)
