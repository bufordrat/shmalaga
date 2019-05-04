{- 
   File      : Galaga.hs
   Copyright : (c) Matt Teichman, 3/15/17 

   Galaga clone, written in Haskell. 

   Main file, runs the game.

-}

module Main where 

import GameState
import GameConstants
import Sprite
import Choreography
import PlayerMoves
import EnemyMoves
import EventHandler
import HighScores
import Graphics.Gloss
import Graphics.Gloss.Data.Picture
import Graphics.Gloss.Interface.IO.Game 
import Data.Fixed

window :: Display 
window = InWindow "Galaga" screenSize (10,10)


-- The number of frames per second to render. 

main :: IO () 
main = playIO window black fps initState GameState.render eventHandler gameLoop 




--  The game loop for Galaga

gameLoop :: Float -> GameState -> IO GameState 
gameLoop deltaTime state = case state of
    MainMenu newScore -> return $ update deltaTime $ MainMenu newScore 
    (Game _ _ _ _ _ _ _) ->  do   
        ioUpdate deltaTime $ update deltaTime state
    (GameOver newScore) -> return $ update deltaTime (GameOver newScore)
--    (PlayerDead state) -> return $ update deltaTime (PlayerDead state)
    (HighScores newScore) -> return $ update deltaTime (HighScores newScore)
    Quit -> error "Quitting the Game!"


--  transitions between game states
--  
--    moves all enemies
--    moves all bullets
--    moves player
--    advances the clock
--    removes all dead sprites from the screen
--    kills all sprites that are currently colliding with bullets
--    kills the player if the player is colliding with an enemy 
--      bullet or an enemy
--    otherwise, does nothing

update :: Float -> GameState -> GameState

update deltaTime state@(Game _ _ _ _ _ _ _) 
   =  ((advClockAttackers deltaTime) .
       enemyCarnage . 
       advanceLevel .
       (moveEnmyState deltaTime (eSpeed * 
                                 (fromIntegral (difficulty state))/2.5)) .
       (movePBltState deltaTime (pbSpeed * 
                                 (fromIntegral (difficulty state))/2.5)) .
       (mvPlyrState deltaTime plyrSpeed) .
       (advanceClock deltaTime) .
       sweepState .
       carnage)
      state


-- update for the other kinds of game states is trivial

update deltaTime state@(MainMenu newScore)
   = state

update deltaTime state@(HighScores newScore)
   = state

update deltaTime state@(GameOver newScore)
   = state

update _ state = state 



-- decreases player's life by one if player get hit
--   if player has 1 life left, ends the game

decrementLife :: GameState -> IO GameState
decrementLife state@(Game _ _ _ _ _ _ _)
   | (lives state) > 1 && (playerIsDead state) 
     = return $ ((swapLives ((lives state) - 1)) .
                 (swapScore (score state)))
                (initGame (level2Init (difficulty state)))
   | (lives state) == 1 && (playerIsDead state)
     = do
         writeScore (score state)
         return $ GameOver (score state)
   | otherwise 
     = return state
decrementLife state = return state



-- true of a state just in case the player is dead

playerIsDead :: GameState -> Bool
playerIsDead state 
   = dead (player state)



--  transitions between side-effect-y game states

ioUpdate :: Float -> GameState -> IO GameState
ioUpdate deltaTime state@(Game _ _ _ _ _ _ _)
  = do 
      state1 <- timeToAttack deltaTime state
      stateA <- timeToFire deltaTime state1
      state2 <- decrementLife stateA
      return state2
ioUpdate deltaTime state
   = return state 
