module Visualise where

import qualified Graphics.Gloss as Gloss
import qualified Tracab

translateCoordinates :: Tracab.Coordinates -> (Gloss.Picture -> Gloss.Picture)
translateCoordinates coordinates =
    Gloss.Translate (getPoint $ Tracab.x coordinates) (getPoint $ Tracab.y coordinates)
    where
    getPoint x = (fromIntegral x) / 20
