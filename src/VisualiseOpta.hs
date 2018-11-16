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
    draw frame = Gloss.pictures [ pitchDrawing, drawFrame game frame ]
    advance = advanceModel game
    initialModel = F24.events game
    fps = 1
    display = Gloss.InWindow "Opta in Tracab Coordinates" (800, 600) (10, 10)
    color = Colors.background
    pitchDrawing =
        Gloss.Color Colors.pitch
        $ Gloss.Polygon [ topLeft, topRight, bottomRight, bottomLeft]
    topLeft = (-100, -100)
    topRight = (100, -100)
    bottomRight = (100, 100)
    bottomLeft = (-100, 100)


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
    pictures = [ eventId, typeId, periodId, eventPlace ]
    eventId = Gloss.Translate (-200) 250 $ intLabel "eventId" $ F24.event_id event
    typeId = Gloss.Translate    200  250 $ intLabel "typeId" $ F24.type_id event
    periodId = Gloss.Translate    0  250 $ intLabel "periodId" $ F24.period_id event
    eventPlace =
        case F24.coordinates event of
            Nothing ->
                Gloss.Blank
            Just coordinates ->
                Gloss.Color (Gloss.makeColor 0.1 0.1 0.9 1.0)
                $ Visualise.translateCoordinates coordinates
                $ Gloss.ThickCircle radius thickness
    radius = 5
    thickness = 3

    intLabel :: String -> Int -> Gloss.Picture
    intLabel title value =
        Gloss.Color Colors.writing
        $ Gloss.Scale 0.2 0.2
        $ Gloss.Text (title ++ ": " ++ (show value))


endGame :: Gloss.Picture
endGame
    = Gloss.Translate (-170) (-20) -- shift the text to the middle of the window
    $ Gloss.Scale 0.5 0.5 -- display it half the original size
    $ Gloss.Text "End of Game" -- text to display
