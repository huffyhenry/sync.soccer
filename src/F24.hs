module F24 where

import qualified Tracab
import Prelude hiding (min)
import qualified Data.ByteString as BS
import Text.XML.Light.Input
import Text.XML.Light.Types
import Text.Printf (printf)
import Control.Exception
import Control.Monad (liftM)
import Data.DateTime
import Data.Maybe
import Data.Typeable
import XmlUtils ( attrLookupStrict, attrLookup )
import qualified XmlUtils as Xml

data Game coordinates = Game {
    gid :: Int,
    away_team_id :: Int,
    away_team_name :: String,
    competition_id :: Int,
    competition_name :: String,
    game_date :: DateTime,
    home_team_id :: Int,
    home_team_name :: String,
    matchday :: Int,
    period_1_start :: DateTime,
    period_2_start :: DateTime,
    season_id :: Int,
    season_name :: String,
    events :: [Event coordinates]
}

instance Eq (Game a) where
    g1 == g2 = gid g1 == gid g2

instance Show (Game a) where
    show g = show (gid g) ++ " " ++ home_team_name g ++ " vs " ++ away_team_name g ++ " on " ++ show (game_date g)


data Event coordinates = Event {
    eid :: Int,
    event_id :: Int,
    type_id :: Int,
    period_id :: Int,
    min :: Int,
    sec :: Int,
    player_id :: Maybe Int,
    team_id :: Int,
    outcome :: Maybe Int,
    coordinates :: Maybe coordinates,
    timestamp :: DateTime,
    last_modified :: DateTime,
    qs :: [Q]
}

instance Eq (Event a) where
    e1 == e2 = eid e1 == eid e2

instance Show (Event a) where
    show e = let typeName = eventTypeName e
             in printf "Event %d (%02d:%02d %s)" (eid e) (min e) (sec e) typeName

hasq :: Int -> Event a -> Bool
hasq i e = any (hasQid i) (qs e)

-- FIXME: Consider throwing an exception on duplicated qualifiers
qval :: Int -> Event a -> Maybe String
qval i e = let qq = filter (hasQid i) (qs e)
           in case qq of
                  [] -> Nothing
                  q:_ -> value q

hasQid :: Int -> Q -> Bool
hasQid i q = qualifier_id q == i

data Q = Q {
    qid :: Int,
    qualifier_id :: Int,
    value :: Maybe String
}

instance Eq Q where
    q1 == q2 = qid q1 == qid q2

instance Show Q where
    show q = "Q" ++ show (qualifier_id q) ++ if isNothing (value q) then "" else " (value: " ++ show (value q) ++ ")"


-- F24 data may be loaded from different resources
class F24Loader a where
    loadGame :: a -> IO (Game F24Coordinates)


data F24Coordinates = F24Coordinates {
    xPercentage :: Float,
    yPercentage :: Float
}

loadGameFromFile :: String -> IO (Game F24Coordinates)
loadGameFromFile filepath = do
    root <- Xml.loadXmlFromFile filepath
    xml <- BS.readFile filepath
    return (makeGame (head $ Xml.getAllChildren root))


makeQ :: Element -> Q
makeQ el = Q { qid = attrLookupStrict el read "id",
               qualifier_id = attrLookupStrict el read "qualifier_id",
               value = attrLookup el read "value" }

makeEvent :: Element -> Event F24Coordinates
makeEvent el =
    Event
        { eid = attrLookupStrict el read "id",
            event_id = attrLookupStrict el read "event_id",
            type_id = attrLookupStrict el read "type_id",
            period_id = attrLookupStrict el read "period_id",
            min = attrLookupStrict el read "min",
            sec = attrLookupStrict el read "sec",
            player_id = attrLookup el read "player_id",
            team_id = attrLookupStrict el read "team_id",
            outcome = attrLookup el read "outcome",
            coordinates = coordinates,
            timestamp = attrLookupStrict el read "timestamp",
            last_modified = attrLookupStrict el read "last_modified",
            qs = map makeQ $ Xml.getChildrenWithQName "Q" el
            }
    where
    coordinates =
        do x <- attrLookup el read "x"
           y <- attrLookup el read "y"
           return $ F24Coordinates { xPercentage = x, yPercentage = y }

makeGame :: Element -> Game F24Coordinates
makeGame el = Game { gid = attrLookupStrict el read "id",
                     away_team_id = attrLookupStrict el read "away_team_id",
                     away_team_name = attrLookupStrict el id "away_team_name",
                     competition_id = attrLookupStrict el read "competition_id",
                     competition_name = attrLookupStrict el id "competition_name",
                     game_date = attrLookupStrict el read "game_date",
                     home_team_id = attrLookupStrict el read "home_team_id",
                     home_team_name = attrLookupStrict el id "home_team_name",
                     matchday = attrLookupStrict el read "matchday",
                     period_1_start = attrLookupStrict el read "period_1_start",
                     period_2_start = attrLookupStrict el read "period_2_start",
                     season_id = attrLookupStrict el read "season_id",
                     season_name = attrLookupStrict el id "season_name",
                     events = map makeEvent $ Xml.getChildrenWithQName "Event" el
                   }


isAwayTeam :: Game a -> Event b -> Bool
isAwayTeam game event =
    (team_id event) == (away_team_id game)

isHomeTeam :: Game a -> Event b -> Bool
isHomeTeam game event =
    (team_id event) == (home_team_id game)

convertGameCoordinates :: Tracab.TeamKind -> Tracab.Metadata -> Game F24Coordinates -> Game Tracab.Coordinates
convertGameCoordinates flippedFirstHalf metaData game =
    game { events = map convertEvent (events game) }
    where
    convertEvent event =
        event { coordinates = liftM convertCoordinates $ coordinates event }
        where
        convertCoordinates coords =
            Tracab.Coordinates
                { Tracab.x = convertedX
                , Tracab.y = convertedY
                }
            where
            isInPenaltyBox =
                (optaX <= 17.0 || optaX >= 83) &&
                (21.1 <= optaY && optaY <= 78.9)

            convertedX = convertCentiMeters pitchLength xCentiMeters
            pitchLength = Tracab.pitchSizeX metaData
            optaX = xPercentage coords
            xCentiMeters =
                -- So note, for the x coordinate to be treated as in the penalty box
                -- the event has to be entirely within a penalty box with respect to
                -- both x and y coordinates, not just the x. Same is true below for y
                -- (which is less controversial).
                case isInPenaltyBox of
                    False ->
                        -- The tracab pitch length is given in *meters* but the x-coordinate of
                        -- a frame is given in centi-meters. The optaX is given in percentage so normally
                        -- to find out how far along the pitch we'd find out what 1% of the pitch is and
                        -- multiply it by the optaX (a percentage). However 1% is just the pitchLength divided
                        -- by 100, but since the pitch length is given in meters it's already divided by 100.
                        optaX * pitchLength
                    True ->
                        case optaX <= 17.0 of
                            True ->
                                yards * 91.44
                                where
                                yards = optaX * 18.0 / 17.0
                            False ->
                                100 * meters
                                where
                                meters = pitchLength - metersFromOppGoal
                                metersFromOppGoal = 0.9144 * yardsFromOppGoal
                                yardsFromOppGoal = percentageFromOppGoal * (18.0 / 17.0)
                                percentageFromOppGoal = 100.0 - optaX

            convertedY = convertCentiMeters pitchWidth yCentiMeters
            pitchWidth = Tracab.pitchSizeY metaData
            optaY = yPercentage coords
            yCentiMeters =
                case isInPenaltyBox of
                    False ->
                        -- See above, basically the pitchWidth is given in meters but the y-coordinate we
                        -- want is in centi-meters.
                        optaY * pitchWidth
                    True ->
                        -- A penalty box extends 18y from both sides of the goal which is itself 8 y.
                        -- For opta the 'bottom' is at 21.1 and the 'top' at 78.9.
                        yards * 91.44
                        where
                        yards = optaY * (18.0 + 18.0 + 8) / (78.9 - 21.1)

            convertCentiMeters pitchSize centimeters =
                -- Again the pitch size is given in meters so we have to multiply by 100.
                perhapsFlipFactor * (round $ centimeters - (pitchSize * 100 / 2.0))


        perhapsFlipFactor =
            case eventTeam == flippedTeam of
                True -> (-1)
                False -> 1


        -- NOTE: We're going to have to worry about periods of extra-time, I think we have to pass in which
        -- team was left-to-right in the first period of extra-time because it's not necessarily flipped from
        -- the second-half of normal time.
        isFirstHalf = (period_id event) == 1
        -- All of OPTA's events assume that the team associated with the event are playing from left-to-right.
        -- However, tracab's coordinates do flip in this manner, so each event must be either flipped or not.
        -- Once we have determined which team was playing left-to-right in the first half then we know whether
        -- we need to flip the coordinates from that event or not.
        -- Note that we have to flip the *y* as well as the x.
        flippedTeam =
                case isFirstHalf of
                    True ->
                        flippedFirstHalf
                    False ->
                        Tracab.oppositionKind flippedFirstHalf
        eventTeam =
            -- Note this kind of assumes that if the event is not a home-team-event it's an away-team-event
            -- I'm not sure if there are any 'neutral events' and if so, how we would determine whether it
            -- needs flipped or not.
            case isHomeTeam game event of
                True -> Tracab.Home
                False -> Tracab.Away




eventTypeName :: Event a -> String
eventTypeName event =
    case type_id event of
        1 -> "Pass"
        2 -> "Offside pass"
        3 -> "Take on"
        4 -> "Foul"
        5 -> "Ball out"
        6 -> "Corner Awrd"
        7 -> "Tackle"
        8 -> "Interception"
        9 -> "Turnover"
        10 -> "Save"
        11 -> "Claim"
        12 -> "Clearance"
        13 -> "Miss"
        14 -> "Woodwork"
        15 -> "AttemptSaved"
        16 -> "Goal"
        17 -> "Card"
        18 -> "Sub-Off"
        19 -> "Sub-On"
        20 -> "Player Retired"
        21 -> "Player returns"
        22 -> "Player->Gk"
        23 -> "Gk -> Player"
        24 -> "Conditions change"
        25 -> "Official change"
        -- 26 really missing in documentation dito the others that are commented
        27 -> "Start delay"
        28 -> "End Delay"
        -- 29
        30 -> "Period end"
        -- 31
        32 -> "Period start"
        -- 33
        34 -> "Team setup"
        35 -> "Player change position"
        36 -> "Player changed jersey number"
        37 -> "Match end"
        38 -> "Temp goal"
        39 -> "Temp attempt"
        40 -> "Formation change"
        41 -> "Gk ball punch"
        42 -> "Good skill"
        43 -> "Deleted event"
        44 -> "Aerial"
        45 -> "Failed challenge"
        -- 46
        47 -> "Card rescinded"
        -- 48
        49 -> "Ball recovery"
        50 -> "Dispossesed"
        51 -> "Error"
        52 -> "Keeper pick up"
        53 -> "Cross not claimed"
        54 -> "Smother"
        55 -> "Offside provoked"
        56 -> "Sheild ball out"
        57 -> "Foul throw"
        58 -> "Penalty faced"
        59 -> "Keeper sweeper"
        60 -> "Chance missed"
        61 -> "Bad touch"
        -- 62
        63 -> "Temp save"
        64 -> "Resume"
        65 -> "Contentious decision"

        unknown -> "Unknown: " ++ show unknown
