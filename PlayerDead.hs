{- 
   File      : PlayerDead.hs
   Copyright : (c) Matt Teichman, 3/15/17 

   Module for handling the Player Dead screen.

-}


module PlayerDead where


import Graphics.Gloss
import Graphics.Gloss.Data.Picture


-- displays a message: "You got killed!"

pdMessage :: IO Picture
pdMessage 
   = return $ color white                       
            $ translate (-250) (75)
            $ scale 0.25 0.25
            $ text ("Game Over!")


-- displays a message: "Press any key to continue..."

pdPAK :: IO Picture
pdPAK
   = return $ color white                       
            $ translate (-250) (-75)
            $ scale 0.25 0.25
            $ text ("Press any key to continue...")


