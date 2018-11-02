-- A nifty animated fractal of a tree, superimposed on a background
--    of three red rectangles.
import Graphics.Gloss
import qualified Tracab
import qualified Data.IntMap as Map
import System.IO  (openFile, hGetContents, hClose, IOMode(ReadMode))
import System.Environment (getArgs)

main :: IO ()
main
 =  do
    (filename : clArguments) <- getArgs
    dataLines <- Tracab.parseFile filename
    animate (InWindow "Tracab" (800, 600) (5,5)) (greyN 0.2) (frame dataLines)

frame :: [ Tracab.DataLine ] -> Float -> Picture
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

-- Produce one frame of the animation.
frameTree :: Float -> Picture
frameTree timeS
 = Pictures
     -- the red rectangles
    [ Translate 0 150     backRec
    , Translate 0 0        backRec
    , Translate 0 (-150)    backRec

    -- the tree
     , Translate 0 (-150) $    treeFrac 7 timeS
    ]


-- One of the red backing rectangles, with a white outline.
backRec :: Picture
backRec
 = Pictures
    [ Color red     (rectangleSolid 400 100)
     , Color white    (rectangleWire  400 100) ]


-- The color for the outline of the tree's branches.
treeOutline :: Color
treeOutline    = makeColor 0.3 0.3 1.0 1.0


-- The color for the shading of the tree's branches.
--    The Alpha here is set to 0.5 so the branches are partly transparent.
treeColor :: Color
treeColor    = makeColor 0.0 1.0 0.0 0.5


-- The tree fractal.
--    The position of the branches changes depending on the animation time
--    as well as the iteration number of the fractal.
treeFrac :: Int -> Float -> Picture
treeFrac 0 timeS = Blank
treeFrac n timeS
 = Pictures
    [ Color treeColor     $ rectangleUpperSolid 20 300
    , Color treeOutline    $ rectangleUpperWire  20 300
     , Translate 0 30
        $ Rotate  (200 * sin timeS / (fromIntegral n) )
        $ Scale   0.9 0.9
        $ treeFrac (n-1) timeS

    , Translate 0 70
        $ Rotate  (-200 * sin timeS / (fromIntegral n))
        $ Scale      0.8 0.8
        $ treeFrac (n-1) timeS
    ]