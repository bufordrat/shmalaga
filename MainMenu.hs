{- 
   File      : MainMenu.hs
   Copyright : (c) Matt Teichman, 3/15/17 

   Module for handling the main menu in Galaga.hs.

-}


module MainMenu where


import Graphics.Gloss
import Graphics.Gloss.Data.Picture
import Sprite

-- displays a message: "Main Menu"

mmMessage :: IO Picture
mmMessage 
   = return $ color white                       
            $ translate (-300) (0)
            $ scale 0.25 0.25
            $ text ("Main Menu")


-- displays a message: "press 's' to start"

mmS :: IO Picture
mmS
   = return $ color white                       
            $ translate (-300) (-100)
            $ scale 0.25 0.25
            $ text ("press 's' to start")



-- displays a message: "press 'q' to quit"

mmQ :: IO Picture
mmQ
   = return $ color white                       
            $ translate (-300) (-150)
            $ scale 0.25 0.25
            $ text ("press 'q' to quit")

-- displays a message: "press 'h' to view the high scores"

mmH :: IO Picture
mmH
   = return $ color white                       
            $ translate (-300) (-200)
            $ scale 0.25 0.25
            $ text ("press 'h' to view the high scores")

resizeLogo :: (Float, Float) -> (Float, Float) -> IO Picture
resizeLogo scaleXY transXY = do
                pic <- galPic
                return $ ((scale (fst scaleXY) (snd scaleXY)) . 
                          (translate (fst transXY) (snd transXY))) 
                         pic

galPic :: IO Picture
galPic = loadBMP "images/models/galaga.bmp"
