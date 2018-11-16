import qualified Graphics.Gloss as Gloss
import qualified Graphics.Gloss.Interface.Pure.Simulate as Simulate
import qualified Tracab
import qualified F24
import qualified Data.IntMap as Map
import System.IO  (openFile, hGetContents, hClose, IOMode(ReadMode))
import System.Environment (getArgs)
import qualified Visualise
import qualified Colors

main :: IO ()
main
 =  do
    (metafile : datafile : clArguments) <- getArgs
    meta <- Tracab.parseMetaFile metafile
    f24game <- F24.loadGameFromFile datafile
    let game = F24.convertCoordinates meta f24game
    animate meta game

animate :: Tracab.Metadata -> F24.Game Tracab.Coordinates -> IO ()
animate meta game =
    Gloss.simulate display color fps initialModel draw advance
    where
    draw frame =
        Gloss.Scale 5 5
        $ Gloss.pictures [ pitchDrawing, drawFrame game frame ]
    advance = advanceModel game
    initialModel = F24.events game
    fps = 1
    display = Gloss.InWindow "Opta in Tracab Coordinates" (800, 600) (10, 10)
    color = Colors.background
    pitchDrawing =
        Gloss.Pictures
            [ grassRectangle
            , pitchLine $ Gloss.circle 0.1
            , pitchLine $ Gloss.circle 10
            , pitchLine $ Gloss.line [ (0, top), (0, bottom) ]
            , pitchLine $ Gloss.line [ topLeft, topRight, bottomRight, bottomLeft, topLeft ]
            ]

    pitchLine = Gloss.Color Gloss.white

    grassRectangle =
        -- Just make the grass rectangle a bit bigger than the pitch.
        Gloss.Scale 1.1 1.1
        $ Gloss.Color Colors.pitch
        $ Gloss.Polygon [ topLeft, topRight, bottomRight, bottomLeft]

    topLeft = (left, top)
    topRight = (right, top)
    bottomRight = (right, bottom)
    bottomLeft = (left, bottom)

    top = halfPitchWidth
    bottom = 0 - halfPitchWidth
    left = 0 - halfPitchLength
    right = halfPitchLength

    halfPitchLength = (Tracab.pitchSizeX meta) / 2.0
    halfPitchWidth = (Tracab.pitchSizeY meta) / 2.0

type Model = [ F24.Event Tracab.Coordinates ]

advanceModel :: F24.Game Tracab.Coordinates -> Simulate.ViewPort -> Float -> Model -> Model
advanceModel _game _viewport _time [] = []
advanceModel _game _viewport _time (_:rest) = rest

drawFrame :: F24.Game Tracab.Coordinates ->  Model -> Gloss.Picture
drawFrame _game [] = endGame
drawFrame game (event : _) = drawEvent game event

drawEvent :: F24.Game Tracab.Coordinates -> F24.Event Tracab.Coordinates -> Gloss.Picture
drawEvent game event =
    Gloss.Pictures pictures
    where
    pictures = [ eventId, typeName, periodId, teamLabel, eventPlace ]
    eventId = Gloss.Translate  (-80) 50 $ intLabel "eventId" $ F24.event_id event
    periodId = Gloss.Translate (-20) 50 $ intLabel "periodId" $ F24.period_id event
    typeName = Gloss.Translate   30  50 $ Gloss.Color Colors.writing $ text (F24.eventTypeName event)
    teamLabel =
        case F24.isHomeTeam game event of
            True ->
                makeTeamLabel (-80) Colors.homeTeam $ F24.home_team_name game
            False ->
                case F24.isAwayTeam game event of
                    True ->
                        makeTeamLabel 20 Colors.awayTeam $ F24.away_team_name game
                    False ->
                        makeTeamLabel 0 Colors.neutralTeam "no team"

    makeTeamLabel x color name =
        Gloss.Translate x 40 $ Gloss.Color color $ text name


    eventPlace =
        case F24.coordinates event of
            Nothing ->
                Gloss.Blank
            Just coordinates ->
                Gloss.Color (Gloss.makeColor 0.1 0.1 0.9 1.0)
                $ Visualise.translateCoordinates coordinates
                $ Gloss.ThickCircle radius thickness
    radius = 0.5
    thickness = 0.3

    text s = Gloss.Scale 0.05 0.05 $ Gloss.Text s

    intLabel :: String -> Int -> Gloss.Picture
    intLabel title value =
        Gloss.Color Colors.writing
        $ text (title ++ ": " ++ (show value))


endGame :: Gloss.Picture
endGame
    = Gloss.Translate (-170) (-20) -- shift the text to the middle of the window
    $ Gloss.Scale 0.5 0.5 -- display it half the original size
    $ Gloss.Text "End of Game" -- text to display
