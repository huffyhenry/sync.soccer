import Graphics.Gloss
import qualified Tracab
import qualified Data.IntMap as Map
import System.IO  (openFile, hGetContents, hClose, IOMode(ReadMode))
import System.Environment (getArgs)
import qualified Visualise

main :: IO ()
main
 =  do
    (metafile : datafile : clArguments) <- getArgs
    meta <- Tracab.parseMetaFile metafile
    frames <- Tracab.parseDataFile meta datafile
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
    $ Visualise.translateCoordinates (Tracab.coordinates position)
    $ ThickCircle radius thickness
    where
        radius = 5

renderPlayer :: Tracab.Position -> Picture
renderPlayer position =
    renderPosition 1 teamColor position
    where
    teamColor =
        case Tracab.mTeam position of
            Just Tracab.Home -> makeColor 0.1 0.1 0.9 1.0
            Just Tracab.Away -> makeColor 0.1 0.9 0.1 1.0
            Nothing -> makeColor 0.9 0.9 0.1 1.0

renderBall :: Tracab.Position -> Picture
renderBall position =
    renderPosition 3 ballColor position
    where
    ballColor = makeColor 0.5 0.5 0.5 1.0

