module Colors where

import qualified Graphics.Gloss as Gloss

makeColor :: Int -> Int -> Int -> Float -> Gloss.Color
makeColor r g b o =
    Gloss.makeColor (norm r) (norm g) (norm b) o
    where
    norm x = (fromIntegral x) / 255.0


pitch :: Gloss.Color
pitch = makeColor 9 78 17 1.0

background :: Gloss.Color
background = makeColor 7 36 10 1.0

writing :: Gloss.Color
writing = makeColor 222 238 223 1.0

eventColor :: Gloss.Color
eventColor = makeColor 221 22 255 1.0

neutralTeam :: Gloss.Color
neutralTeam = makeColor 244 234 234 1.0

homeTeam :: Gloss.Color
homeTeam = makeColor 202 17 15 1.0

awayTeam :: Gloss.Color
awayTeam = makeColor 60 158 198 1.0

