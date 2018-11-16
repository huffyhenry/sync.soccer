import qualified Graphics.Gloss as Gloss
import qualified Graphics.Gloss.Interface.Pure.Simulate as Simulate
import qualified Tracab
import qualified F24
import qualified Data.IntMap as Map
import System.IO  (openFile, hGetContents, hClose, IOMode(ReadMode))
import System.Environment (getArgs)

main :: IO ()
main
 =  do
    (metafile : datafile : clArguments) <- getArgs
    meta <- Tracab.parseMetaFile metafile
    f24game <- F24.loadGameFromFile datafile
    let game = F24.convertCoordinates meta f24game
    animate game

animate :: F24.Game Tracab.Coordinates -> IO ()
animate game =
    Gloss.simulate display color fps initialModel draw advance
    where
    draw = drawFrame game
    advance = advanceModel game
    initialModel = F24.events game
    fps = 1
    display = Gloss.InWindow "Tracab" (800, 600) (5,5)
    color = Gloss.greyN 0.2

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
    pictures = [ eventId, typeId, periodId ]
    eventId = Gloss.Translate (-200) 250 $ intLabel "eventId" $ F24.event_id event
    typeId = Gloss.Translate    200  250 $ intLabel "typeId" $ F24.type_id event
    periodId = Gloss.Translate    0  250 $ intLabel "periodId" $ F24.period_id event
    intLabel :: String -> Int -> Gloss.Picture
    intLabel title value =
        Gloss.Scale 0.2 0.2 $ Gloss.Text (title ++ ": " ++ (show value))


endGame :: Gloss.Picture
endGame
    = Gloss.Translate (-170) (-20) -- shift the text to the middle of the window
    $ Gloss.Scale 0.5 0.5 -- display it half the original size
    $ Gloss.Text "End of Game" -- text to display
