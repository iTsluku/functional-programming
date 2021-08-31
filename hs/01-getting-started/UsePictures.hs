module UsePictures where

--  import only one, to avoid conflicts
import Pictures
--import PicturesSVG

--  printPicture (rotateHorse horse)
rotateHorse :: Picture -> Picture
rotateHorse h = rotate h

--  printPicture (invertHorseColour horse)
invertHorseColour :: Picture -> Picture
invertHorseColour h = invertColour h

data Colour = White | Black deriving Show

p = printPicture
horseBlack = invertHorseColour horse    --  horse is white

-- p (whiteHorse horse White) = horse
-- p (whiteHorse horseBlack Black) = horse
whiteHorse :: Picture -> Colour -> Picture
whiteHorse h White = h
whiteHorse h Black = invertHorseColour h
