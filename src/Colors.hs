module Colors where

import qualified Graphics.Gloss as Gloss

makeColor :: Int -> Int -> Int -> Float -> Gloss.Color
makeColor r g b o =
    Gloss.makeColor (norm r) (norm g) (norm b) o
    where
    norm x = (fromIntegral x) / 255.0


pitch :: Gloss.Color
pitch = makeColor 9 78 17 1.0
