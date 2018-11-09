module F24 where

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

data Game = Game {
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
    events :: [Event]
}

instance Eq Game where
    g1 == g2 = gid g1 == gid g2

instance Show Game where
    show g = show (gid g) ++ " " ++ home_team_name g ++ " vs " ++ away_team_name g ++ " on " ++ show (game_date g)


data Event = Event {
    eid :: Int,
    event_id :: Int,
    type_id :: Int,
    period_id :: Int,
    min :: Int,
    sec :: Int,
    player_id :: Maybe Int,
    team_id :: Int,
    outcome :: Maybe Int,
    x :: Maybe Float,
    y :: Maybe Float,
    timestamp :: DateTime, -- FIXME Should be more granular timestamp type.
    last_modified :: DateTime,
    qs :: [Q]
}

instance Eq Event where
    e1 == e2 = eid e1 == eid e2

instance Show Event where
    show e = let t = show (type_id e)
                 i = show (eid e)
                 p = maybe "" (\pid -> "by player " ++ show pid) (player_id e)
                 c = show (team_id e)
             in "E[" ++ i ++ "]" ++ " of type " ++ t ++ " for " ++ c ++ p

hasq :: Int -> Event -> Bool
hasq i e = any (\q -> qualifier_id q == i) (qs e)

-- FIXME: Consider throwing an exception on duplicated qualifiers
qval :: Int -> Event -> Maybe String
qval i e = let qq = filter (\q -> qualifier_id q == i) (qs e)
           in case qq of
                  [] -> Nothing
                  q:_ -> value q

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
    loadGame :: a -> IO Game




loadGameFromFile :: String -> IO Game
loadGameFromFile filepath = do
    root <- Xml.loadXmlFromFile filepath
    xml <- BS.readFile filepath
    return (makeGame (head $ Xml.getAllChildren root))


makeQ :: Element -> Q
makeQ el = Q { qid = attrLookupStrict el read "id",
               qualifier_id = attrLookupStrict el read "qualifier_id",
               value = attrLookup el read "value" }

makeEvent :: Element -> Event
makeEvent el = Event { eid = attrLookupStrict el read "id",
                       event_id = attrLookupStrict el read "event_id",
                       type_id = attrLookupStrict el read "type_id",
                       period_id = attrLookupStrict el read "period_id",
                       min = attrLookupStrict el read "min",
                       sec = attrLookupStrict el read "sec",
                       player_id = attrLookup el read "player_id",
                       team_id = attrLookupStrict el read "team_id",
                       outcome = attrLookup el read "outcome",
                       x = attrLookup el read "x",
                       y = attrLookup el read "y",
                       timestamp = attrLookupStrict el read "timestamp",
                       last_modified = attrLookupStrict el read "last_modified",
                       qs = map makeQ $ Xml.getChildrenWithQName "Q" el
                     }

makeGame :: Element -> Game
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