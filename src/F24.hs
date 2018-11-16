module F24 where

import qualified Tracab
import Prelude hiding (min)
import qualified Data.ByteString as BS
import Text.XML.Light.Input
import Text.XML.Light.Types
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
    timestamp :: DateTime, -- FIXME Should be more granular timestamp type.
    last_modified :: DateTime,
    qs :: [Q]
}

instance Eq (Event a) where
    e1 == e2 = eid e1 == eid e2

instance Show (Event a) where
    show e = let t = show (type_id e)
                 i = show (eid e)
                 p = maybe "" (\pid -> "by player " ++ show pid) (player_id e)
                 c = show (team_id e)
             in "E[" ++ i ++ "]" ++ " of type " ++ t ++ " for " ++ c ++ p

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

convertCoordinates :: Tracab.Metadata -> Game F24Coordinates -> Game Tracab.Coordinates
convertCoordinates metaData game =
    game { events = map convertEvent (events game) }
    where
    convertEvent event =
        event { coordinates = liftM convertCoordinates $ coordinates event }
        where
        convertCoordinates coords =
            Tracab.Coordinates
                { Tracab.x = convertX $ xPercentage coords
                , Tracab.y = convertY $ yPercentage coords
                }

        -- We assume that the home team is playing from left to right, it doesn't actually
        -- matter as long as we consistently flip the pitch for *one* team or the other
        -- however, we do need to do this properly when we actually combine the event with
        -- the tracab locations. Note that we have to flip the *y* as well as the x.
        perhapsFlipFactor =
            case isAwayTeam game event of
                True ->
                    (-1)
                False ->
                    1

        convertX = convertUnit $ Tracab.pitchSizeX metaData
        convertY = convertUnit $ Tracab.pitchSizeY metaData
        convertUnit pitchUnit percentage =
            perhapsFlipFactor * (round $ (percentage * pitchUnit / 100.0) - (pitchUnit / 2.0))

eventTypeName :: Event a -> String
eventTypeName event =
    case type_id event of
        1 -> "Pass"
        unknown -> "Unknown: " ++ show unknown