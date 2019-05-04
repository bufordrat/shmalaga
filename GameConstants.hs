{- 
   File      : GameConstants.hs
   Copyright : (c) Matt Teichman, 3/15/17 

   Constants for use in intializing the game state in Galaga.hs,
   and for controlling the speed of the sprites.

-}


module GameConstants where 

import Graphics.Gloss


type Size = (Int,Int)

screenSize :: Size 
screenSize = (scrWidth, scrHeight)



-- frames per second

fps :: Int 
fps = 60 



-- width of the window

scrWidth :: Int
scrWidth = 800


-- height of the window

scrHeight :: Int
scrHeight = 600


-- room sprites have to go offscreen before they're considered
--   out of bounds

pad :: Float
pad = 50.0


-- player's speed

plyrSpeed :: Float
plyrSpeed = 450.0


-- speed of the enemies

eSpeed :: Float
eSpeed = 200.0


-- speed of the player's bullets

pbSpeed :: Float
pbSpeed = 1200.0



-- how often enemies attack in seconds

attackInterval :: Float
attackInterval = 3.0


-- how often the level changes

levelInterval :: Float
levelInterval = 30.0


-- how close two sprites have to be in order to count as colliding

collThreshold :: Float
collThreshold = 17.0



