import Graphics.Gloss
import qualified Tracab
import qualified Data.IntMap as Map
import System.IO  (openFile, hGetContents, hClose, IOMode(ReadMode))
import System.Environment (getArgs)

main :: IO ()
main
 =  do
    (filename : clArguments) <- getArgs
    frames <- Tracab.parseDataFile filename
    animate (InWindow "Tracab" (800, 600) (5,5)) (greyN 0.2) (frame frames)

frame :: Tracab.Frames -> Float -> Picture
frame dataLines time =
    case currentTime >= length dataLines of
        True -> endGame
        False -> Pictures pictures
    where
        pictures =  ballPicture : playerPictures
        ballPicture = renderBall $ Tracab.ballPosition currentLine
        playerPictures =
            map renderPlayer players
        players = Map.elems $ Tracab.positions currentLine
        currentLine = dataLines !! currentTime

        currentTime :: Int
        currentTime = round (10 * time)

endGame :: Picture
endGame
    = Translate (-170) (-20) -- shift the text to the middle of the window
    $ Scale 0.5 0.5 -- display it half the original size
    $ Text "End of Game" -- text to display

renderPosition :: Float -> Color -> Tracab.Position -> Picture
renderPosition thickness teamColor position =
    Color teamColor
    $ Translate (getPoint $ Tracab.x position) (getPoint $ Tracab.y position)
    $ ThickCircle radius thickness
    where
        getPoint x = (fromIntegral x) / 20
        radius = 5


renderPlayer :: Tracab.Position -> Picture
renderPlayer position =
    renderPosition 1 teamColor position
    where
    teamColor =
        case Tracab.teamId position of
            0 -> makeColor 0.1 0.1 0.9 1.0
            1 -> makeColor 0.1 0.9 0.1 1.0
            3 -> makeColor 0.9 0.9 0.1 1.0
            _ -> makeColor 0.9 0.9 0.9 1.0

renderBall :: Tracab.Position -> Picture
renderBall position =
    renderPosition 3 ballColor position
    where
    ballColor = makeColor 0.5 0.5 0.5 1.0

