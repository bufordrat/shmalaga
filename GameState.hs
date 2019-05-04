{- 
   File      : GameState.hs
   Copyright : (c) Matt Teichman, 3/15/17 

   Module defining game states for use in Galaga, including
   a render function for displaying them to the screen.
    
-}


module GameState where

import Graphics.Gloss
import Graphics.Gloss.Data.Picture
import Graphics.Gloss.Interface.IO.Game 
import Sprite
import HighScores
import GameConstants
import MainMenu
import GameOver
import PlayerDead
import Data.Fixed


-- game state datatype

data GameState    = MainMenu Int
                  | Game {player :: Sprite,
                          enemies :: [Sprite],
                          bullets :: [Sprite],
                          score :: Int,
                          difficulty :: Int,
                          lives :: Int,
                          clock :: Float} 
                  | HighScores Int 
                  | GameOver Int
                  | Quit
     deriving (Show, Eq)



-- render takes a game state and renders it to the screen

render :: GameState -> IO Picture 
render (MainMenu newScore) = ioPictures [mmMessage, 
                                         mmS, 
                                         mmQ, 
                                         mmH, 
                                         resizeLogo (0.25, 0.25) (0, 550)]

render state@(Game _ _ _ _ _ _ _) = 
   ioPictures $ listify (Sprite.render $ player state)
                        (map (Sprite.render) $ enemies state)
                        (map (Sprite.render) $ bullets state)
                        ([displayScore state, 
                          displayLives state, 
                          displayLevel state, displayEnemies state])

render (HighScores newScore) = ioPictures [hiScoresPic newScore, 
                                           hiScoresMessage, 
                                           pressAnyKey]

render (GameOver newScore)  =  ioPictures [gameOverMessage,
                                           gameOverPAK]   

-- render (PlayerDead gameState) = ioPictures [pdMessage,
--                                            pdPAK] 

render _ = return Blank



-- takes an element and two lists, appends the two lists, and 
--   conses the element onto the result

listify :: a -> [a] -> [a] -> [a] -> [a]
listify n m o p = n:(m ++ o ++ p)



-- turns the score of a game state into an IO Picture

displayScore :: GameState -> IO Picture
displayScore state@(Game _ _ _ _ _ _ _)
   = return $ color white                       
            $ translate (-380) (-280)
            $ scale 0.1 0.1
            $ text ("Score: "++show (score state))
displayScore state = error "only for game state"


-- turns the player's remaining lives into an IO Picture

displayLives :: GameState -> IO Picture
displayLives state@(Game _ _ _ _ _ _ _) 
   = return $ color white                       
            $ translate (320) (-280)
            $ scale 0.1 0.1
            $ text ("Lives: "++show (lives state))
displayLives state = error "only for game state"


-- displays the difficulty level at the bottom of the screen

displayLevel :: GameState -> IO Picture
displayLevel state@(Game _ _ _ _ _ _ _) 
   = return $ color white                       
            $ translate (-20) (-280)
            $ scale 0.1 0.1
            $ text ("Level: "++show (difficulty state))
displayLevel state = error "only for game state"


-- displays the clock (for debugging purposes only)

displayClock :: GameState -> IO Picture
displayClock state@(Game _ _ _ _ _ _ _) 
   = return $ color white                       
            $ translate (-20) (280)
            $ scale 0.1 0.1
            $ text ("Clock: "++show (clock state))
displayClock state = error "only for game state"


-- displays the number of enemies (for debugging purposes only)

displayEnemies :: GameState -> IO Picture
displayEnemies state@(Game _ _ _ _ _ _ _) 
   = return $ color white                       
            $ translate (-20) (280)
            $ scale 0.1 0.1
            $ text ("Enemies: "++show (length (enemies state)))
displayEnemies state = error "only for game state"



-- creates a new enemy of a specified type in a given location

newEnemy :: Character -> (Float, Float) -> Sprite
newEnemy BlueEnemy coords = Sprite BlueEnemy
                                   coords
                                   (0.25, 0.25)
                                   Alive
                                   NoOne
                                   (R, NoMove)
                                   0.0
                                   coords
newEnemy RedEnemy coords = Sprite RedEnemy
                                  coords
                                  (0.25, 0.25)
                                  Alive
                                  NoOne
                                  (R, NoMove)
                                  0.0
                                  coords
newEnemy _ _ = error "newEnemy is for making enemies only"


-- adds an input enemy to the game state

addEnemy :: GameState -> Sprite -> GameState
addEnemy state@(Game _ _ _ _ _ _ _) sprite 
   = Game (player state)
          (sprite:(enemies state))
          (bullets state)
          (score state)
          (difficulty state)
          (lives state)
          (clock state)
addEnemy state _ = state  

-- given a 'character' (i.e. whether it's a red or blue enemy)
--   and a location, maps a state to a new state with an enemy 
--   of the relevant kind in the relevant location

mkEnemy :: Character -> (Float, Float) -> GameState -> GameState
mkEnemy BlueEnemy coords state
   = addEnemy state (newEnemy BlueEnemy coords)
mkEnemy RedEnemy coords state
   = addEnemy state (newEnemy RedEnemy coords)
mkEnemy _ _ _ = error "mkEnemy is for making enemies only"



-- creates a row of enemies
--
--   first argument: vertical distance from center
--   second argument: how far in either direction horizontally the
--                    row should extend

createRow :: Float -> Float -> Character -> GameState -> GameState
createRow width offset character state
   = foldr (mkEnemy character) state (rowInitialize width offset ++ 
                                      rowInitialize width (offset + 50))


-- initializes the 'playing the game' game state

initGame :: GameState -> GameState
initGame state = ((createRow 100 150 RedEnemy) .
                  (createRow 100 50 BlueEnemy)) 
                 (state)

-- the initial game state (main menu)

initState :: GameState 
initState = MainMenu 0


-- the empty game state for level 1

nullGame :: GameState
nullGame = Game playerShip [] [] 0 1 3 0.0


-- the empty states for levels 2, 3, and 4

nullGame2 :: GameState
nullGame2 = Game playerShip [] [] 0 2 3 0.0

nullGame3 :: GameState
nullGame3 = Game playerShip [] [] 0 3 3 0.0

nullGame4 :: GameState
nullGame4 = Game playerShip [] [] 0 4 3 0.0



