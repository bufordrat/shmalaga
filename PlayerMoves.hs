{- 
   File      : PlayerMoves.hs
   Copyright : (c) Matt Teichman, 3/15/17 

   Functions for moving the player in Galaga.hs.

-}


module PlayerMoves where

import GameConstants
import Choreography
import GameState
import Sprite




-- changes direction the player is moving in

plyrChangeDir :: Direction -> Sprite -> Sprite
plyrChangeDir dirxn (Sprite Player l s h o _ c i) 
   = Sprite Player l s h o (dirxn, NoMove) c i
plyrChangeDir dirxn _ = error "plyrChangeDir only defined on Player"



-- changes direction the player is moving in in the state

plyrChgDrState :: Direction -> GameState -> GameState
plyrChgDrState dirxn state@(Game _ _ _ _ _ _ _)
   = Game (plyrChangeDir dirxn (player state))
          (enemies state)
          (bullets state)
          (score state)
          (difficulty state)
          (lives state)
          (clock state)
plyrChgDrState _ state = state


-- moves the player by a tiny amount, depending on the direction
--   the player is facing in

mvPlyr :: Float -> Float -> Direction -> Sprite -> Sprite 
mvPlyr deltaTime playerSpeed dirxn sprite 
   | (xCoord sprite) <= (fromIntegral $ scrWidth)/2 &&
     (xCoord sprite) >= (fromIntegral $ scrWidth)/2 * (-1)
       = case dir sprite of
          (L, _) -> nudge (deltaTime * playerSpeed) 0.0 sprite
          (R, _) -> nudge (deltaTime * playerSpeed) 0.0 sprite 
          _ -> sprite
   | (xCoord sprite) > (fromIntegral $ scrWidth)/2 
       = ((plyrChangeDir NoMove) . 
          (nudge (deltaTime * playerSpeed * 2) 0.0) . 
          (aboutFace)) 
         sprite
   | (xCoord sprite) < (fromIntegral $ scrWidth)/2 * (-1)
       = ((plyrChangeDir NoMove) . 
          (nudge (deltaTime * playerSpeed * 2) 0.0) . 
          (aboutFace)) 
         sprite
   | otherwise = sprite



-- moves the player by a tiny amount in the direction they're facing in
--   within a state

mvPlyrState :: Float -> Float -> GameState -> GameState
mvPlyrState deltaTime playerSpeed state@(Game _ _ _ _ _ _ _)
   = Game (mvPlyr deltaTime 
                  (playerSpeed * (fromIntegral $ difficulty state)/2.5)
                  (fst $ dir $ player state) 
                  (player state))
          (enemies state)
          (bullets state)
          (score state)
          (difficulty state)
          (lives state)
          (clock state)
mvPlyrState _ _ state = state

-- fires a bullet from the player's ship

pFireBullet :: GameState -> GameState
pFireBullet state@(Game _ _ _ _ _ _ _)
   = Game (player state)
          (enemies state)
          ((mkBullet $ player state):(bullets state))
          (score state)
          (difficulty state)
          (lives state)
          (clock state)
pFireBullet state = state


