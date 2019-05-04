{- 
   File      : GameOver.hs
   Copyright : (c) Matt Teichman, 3/15/17 

   Module for handling the high scores in Galaga.hs.

-}


module GameOver where

import Graphics.Gloss
import Graphics.Gloss.Data.Picture



-- displays a message: "Game Over!"

gameOverMessage :: IO Picture
gameOverMessage 
   = return $ color white                       
            $ translate (-250) (75)
            $ scale 0.25 0.25
            $ text ("Game Over!")


-- displays a message: "Press any key to continue..."

gameOverPAK :: IO Picture
gameOverPAK
   = return $ color white                       
            $ translate (-250) (-75)
            $ scale 0.25 0.25
            $ text ("Press any key to continue...")


