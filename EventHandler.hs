{- 
   File      : EventHandler.hs
   Copyright : (c) Matt Teichman, 3/15/17 

   Functions for use in Galaga.hs that manipulate the enemies.
   
   (Note that enemies, bullets, and the player are all of type Sprite.)   

-}

module EventHandler where

import GameState
import Graphics.Gloss.Interface.IO.Game 
import Choreography
import EnemyMoves
import PlayerMoves
import Sprite
import System.Random


-- The event handler handles events coming from the user 

eventHandler :: Event -> GameState -> IO GameState 


-- Pattern matches for Key Events in main menu

eventHandler (EventKey (Char key) Up _ _) state@(MainMenu newScore)
  = case key of 
      's' -> return $ initGame (level2Init 1)
      'q' -> return Quit
      'h' -> return $ HighScores newScore
      _ -> return state 


-- Pattern matches for Key Events while playing a game

eventHandler (EventKey (Char key) Up _ _) state@(Game _ _ _ _ _ _ _)
  = case key of 
      'q' -> return (GameOver (score state))
      '1' -> return $ changeLevel 1 state 
      '2' -> return $ changeLevel 2 state 
      '3' -> return $ changeLevel 3 state 
      '4' -> return $ changeLevel 4 state 
      'p' -> attackersFire state
      _ -> return state 


-- Pattern matches for Special Key Events in the game
--   space to fire
--   left to move left
--   right to move right

eventHandler (EventKey (SpecialKey key) Down _ _) state@(Game _ _ _ _ _ _ _) 
  = case key of  
   KeyLeft  -> return (plyrChgDrState L state) 
   KeyRight -> return (plyrChgDrState R state) 
   KeySpace -> return $ pFireBullet state
   _ -> return state

eventHandler (EventKey (SpecialKey key) Up _ _) state@(Game _ _ _ _ _ _ _)
  = case key of  
   KeyLeft  -> return $ plyrChgDrState NoMove state
   KeyRight -> return $ plyrChgDrState NoMove state
   _ -> return state


-- Pattern matches for Key Events in the High Scores screen

eventHandler (EventKey _ Up _ _) state@(HighScores newScore)
   = return (MainMenu newScore)



-- Pattern matches for Key Events during Game Over screen
--    (transitions to main screen once user presses a key) 

eventHandler (EventKey _ Up _ _) state@(GameOver newScore)
   = return $ MainMenu newScore



-- Pattern matches for mouse events

eventHandler (EventKey (MouseButton _) Up _ (xPos,yPos)) state = do
  putStrLn $  "Mouse Click on (" ++ show xPos ++ ", " ++ show yPos ++ ")" 
  return state 


-- Catch-All pattern  

eventHandler _ state = return state 
