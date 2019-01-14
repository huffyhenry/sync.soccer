module F24 where

import qualified Data.Map.Strict as Map
import qualified Tracab as Tcb
import Prelude hiding (min)
import qualified Data.ByteString as BS
import Text.XML.Light.Types (Element)
import Text.Printf (printf)
import Control.Monad (liftM)
import Data.DateTime
import Data.Maybe
import XmlUtils ( attrLookupStrict, attrLookup, hasAttributeWithValue )
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


type PlayerId = Int

data Event coordinates = Event {
    eid :: Int,
    event_id :: Int,
    type_id :: Int,
    period_id :: Int,
    min :: Int,
    sec :: Int,
    player_id :: Maybe PlayerId,
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

eventTeam :: Game a -> Event b -> Maybe Tcb.TeamKind
eventTeam game event
    | isHomeTeam game event = Just Tcb.Home
    | isAwayTeam game event = Just Tcb.Away
    | otherwise = Nothing


getFlippedHalves :: Tcb.Metadata -> Tcb.Frames Tcb.Positions -> (Maybe Tcb.TeamKind, Maybe Tcb.TeamKind)
getFlippedHalves metaData frames =
    (Map.lookup 1 flippedMap, Map.lookup 2 flippedMap)
    where
    flippedMap = createFlippedTeamMapping metaData frames


createFlippedTeamMapping :: Tcb.Metadata -> Tcb.Frames Tcb.Positions -> Map.Map Int Tcb.TeamKind
createFlippedTeamMapping metaData frames =
    Map.fromList $ map createKeyFlipped tracabPeriods
    where
    tracabPeriods = filter (\p -> (Tcb.startFrame p) /= (Tcb.endFrame p)) (Tcb.periods metaData)

    createKeyFlipped period =
        ( Tcb.periodId period, flipped)
        where
        flipped =
            case filter isKickOff frames of
                [] ->
                    -- This obviously shouldn't happen but the map is strict and you
                    -- may have incomplete data, for example you may be only analysing
                    -- the second half, in which case you don't care about the first half
                    -- and perhaps haven't provided the frames for the first half.
                    Tcb.Away
                kickOffFrame : _ ->
                    Tcb.rightToLeftKickOff kickOffFrame
        kickOffFrameId = Tcb.startFrame period
        isKickOff frame =
            (Tcb.frameId frame) == kickOffFrameId


convertGameCoordinates :: Tcb.Metadata -> Tcb.Frames Tcb.Positions -> Game F24Coordinates -> Game Tcb.Coordinates
convertGameCoordinates metaData frames game =
    game { events = map convertEvent (events game) }
    where
    flippedMap = createFlippedTeamMapping metaData frames
    convertEvent event =
        event { coordinates = liftM convertCoordinates $ coordinates event }
        where
        convertCoordinates coords =
            Tcb.Coordinates
                { Tcb.x = convertedX
                , Tcb.y = convertedY
                }
            where
            isInPenaltyBox =
                (optaX <= 17.0 || optaX >= 83) &&
                (21.1 <= optaY && optaY <= 78.9)

            convertedX = convertCentiMeters pitchLength xCentiMeters
            pitchLength = Tcb.pitchSizeX metaData
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
            pitchWidth = Tcb.pitchSizeY metaData
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
            case Map.lookup (period_id event) flippedMap of
                Just Tcb.Home | isHomeTeam game event ->
                    -1
                Just Tcb.Away | isAwayTeam game event ->
                    -1
                otherwise ->
                    1

-- Whether the event is an on-the-ball event.
-- Consult the list below for the meaning of event type IDs.
isOTB :: Event c -> Bool
isOTB e = elem (type_id e) ([1..16] ++ [41..45] ++ [49..61])


eventTypeName :: Event a -> String
eventTypeName event =
    case type_id event of
        1 -> "Pass"
        2 -> "Offside pass"
        3 -> "Take on"
        4 -> "Foul"
        5 -> "Ball out"
        6 -> "Corner awarded"
        7 -> "Tackle"
        8 -> "Interception"
        9 -> "Turnover"
        10 -> "Save"
        11 -> "Claim"
        12 -> "Clearance"
        13 -> "Miss"
        14 -> "Woodwork"
        15 -> "Attempt saved"
        16 -> "Goal"
        17 -> "Card"
        18 -> "Sub off"
        19 -> "Sub on"
        20 -> "Player retires"
        21 -> "Player returns"
        22 -> "Player becomes GK"
        23 -> "GK becomes player"
        24 -> "Conditions change"
        25 -> "Official change"
        -- 26 missing in documentation ditto the others that are commented out
        27 -> "Start delay"
        28 -> "End delay"
        -- 29
        30 -> "Period end"
        -- 31
        32 -> "Period start"
        -- 33
        34 -> "Team setup"
        35 -> "Player changes position"
        36 -> "Player changes jersey number"
        37 -> "Match end"
        38 -> "Temp goal"
        39 -> "Temp attempt"
        40 -> "Formation change"
        41 -> "GK ball punch"
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
        56 -> "Shield ball out"
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


type ShirtNumber = Int
type ShirtNumbers = Map.Map PlayerId (Tcb.TeamKind, ShirtNumber)


data Metadata = Metadata{
    homeTeam :: TeamData,
    awayTeam :: TeamData,
    shirtNumbers :: ShirtNumbers
}

data TeamData = TeamData {
    players :: [PlayerData]
}



data PlayerData = PlayerData {
    playerRef :: String,
    formationPosition :: String,
    shirtNumber :: ShirtNumber
}


parseMetaFile :: String -> IO Metadata
parseMetaFile filename = do
    root <- Xml.loadXmlFromFile filename
    return $ makeMetadata root

makeMetadata :: Element -> Metadata
makeMetadata element =
    Metadata
      { homeTeam = parseTeamData homeTeamDataElement
      , awayTeam = parseTeamData awayTeamDataElement
      , shirtNumbers = shirtNumbers
      }
    where
    shirtNumbers = Map.union (collectShirtNumbers Tcb.Home homeTeam) (collectShirtNumbers Tcb.Away awayTeam)

    collectShirtNumbers teamKind teamData =
        Map.fromList $ map createPair (players teamData)
        where
        createPair playerData =
            -- Player data looks like: <MatchPlayer Formation_Place="1" PlayerRef="p167040" Position="Goalkeeper" ShirtNumber="1" Status="Start" />
            -- So for some reason the playerRef is in the format `p<player_id>` where the `player_id` part matches up with those given in the Event data
            -- in the actual f24 file. So to parse it we just drop the 'p' and read the rest as an integer.
            -- This player_id part we map to the shirt number, since this should correspond with the shirt-number used in the Tracab file.
            -- So basically here, we are going from the above MatchPlayer element to `(167040, 1)`.
            ( read $ drop 1 $ playerRef playerData
            , (teamKind, shirtNumber playerData)
            )


    homeTeam = parseTeamData homeTeamDataElement
    awayTeam = parseTeamData awayTeamDataElement
    homeTeamDataElement =
        head $ filter (hasAttributeWithValue "Side" "Home") allTeamData

    awayTeamDataElement =
        head $ filter (hasAttributeWithValue "Side" "Away") allTeamData

    allTeamData =
        Xml.getChildrenWithQName "TeamData" matchData
        where
        soccerFeed = element
        soccerDocument = last $ Xml.getChildrenWithQName "SoccerDocument" soccerFeed
        matchData = head $ Xml.getChildrenWithQName "MatchData" soccerDocument

    parseTeamData teamElement =
        TeamData { players = map parsePlayerData $ Xml.getChildrenWithQName "MatchPlayer" lineupElement }
        where
        lineupElement = head $ Xml.getChildrenWithQName "PlayerLineUp" teamElement

    parsePlayerData playerElement =
        PlayerData
            { playerRef = Xml.attrLookupStrict playerElement id "PlayerRef"
            , formationPosition = Xml.attrLookupStrict playerElement id "Position"
            , shirtNumber = Xml.attrLookupStrict playerElement read "ShirtNumber"
            }
