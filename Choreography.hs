{- 
   File      : Choreography.hs
   Copyright : (c) Matt Teichman, 3/15/17 

   Module for use in Galaga.hs that controls all synthetic events
   not directly involving the movement of the player or enemies.

   (Note that enemies, bullets, and the player are all of type Sprite.)
   
-}



module Choreography where

import GameConstants
import GameState
import Sprite
import Data.List
import Debug.Trace
import Data.Fixed


-- true just in case all the enemies are dead

finishedLevel :: GameState -> Bool
finishedLevel state
   = (enemies state) == []


-- moves to the next level just in case all the enemies are dead

advanceLevel :: GameState -> GameState
advanceLevel state
   | finishedLevel state && (difficulty state) < 4
     = changeLevel ((difficulty state) + 1) state
   | finishedLevel state && (difficulty state) == 4
     = changeLevel (difficulty state) state
   | otherwise 
     = state



-- changes the 'playing the same state' to a new level

changeLevel :: Int -> GameState -> GameState
changeLevel level state@(Game _ _ _ _ _ _ _)
   = ((swapPlayer (player state)) . 
      (swapScore (score state)) . 
      (swapClock (clock state)) . 
      (swapLives (lives state))) 
     (initGame (level2Init level))
changeLevel _ state
   = state




--import PlayerMoves

-- moves a bullet a tiny bit in whatever direction it's facing,
--   which is either up or down

moveBullet :: Float -> Float -> Sprite -> Sprite
moveBullet deltaTime bulletSpeed sprite 
   | owner sprite == Player  
     = nudge 0.0 (deltaTime * bulletSpeed) sprite
   | otherwise  
     = nudge 0.0 (deltaTime * (bulletSpeed * 0.55)) sprite


-- transitions a game state to one in which all bullets are 
--   moved a tiny bit

movePBltState :: Float -> Float -> GameState -> GameState
movePBltState deltaTime bulletSpeed state@(Game _ _ _ _ _ _ _) 
   = Game (player state) 
          (enemies state)
          (map (\x -> moveBullet deltaTime bulletSpeed x) 
               (bullets state))
          (score state)
          (difficulty state)
          (lives state)
          (clock state)
movePBltState _ _ state = state


-- transitions a game state into one in which the clock
--   is moved a tiny bit forward

advanceClock :: Float -> GameState -> GameState
advanceClock deltaTime state@(Game _ _ _ _ _ _ _)
   = Game (player state) 
          (enemies state)
          (bullets state)
          (score state)
          (difficulty state)
          (lives state)
          (clock state + deltaTime)
advanceClock _ state = state

-- removes all dead/offscreen sprites from a game state

sweepState :: GameState -> GameState
sweepState state@(Game _ _ _ _ _ _ _)
   = Game (player state)
          (sweep $ enemies state)
          (sweep $ bullets state)
          (score state)
          (difficulty state)
          (lives state)
          (clock state)
sweepState state = state


-- removes all dead/offscreen sprites from a list

sweep :: [Sprite] -> [Sprite] 
sweep sprites = filter (\x -> not (dead x) 
                              && not (outOfBounds x)
                              && not (hidden x)) sprites



-- relation that holds just in case two sprites have the
--   same home coordinates

sameHome :: Sprite -> Sprite -> Bool
sameHome sprite1 sprite2 = initPos sprite1 == initPos sprite2




-- creates a bullet in the location of the input sprite
--   player bullets face up; enemy bullets face down

mkBullet :: Sprite -> Sprite

mkBullet sprite@(Sprite Player _ _ _ _ _ _ _)
   = Sprite Bullet
            (xCoord sprite, yCoord sprite)
            (0.25, 0.25)
            Alive
            Player
            (NoMove, U)
            (0.0)
            (xCoord sprite, yCoord sprite) 

mkBullet sprite@(Sprite RedEnemy _ _ _ _ _ _ _)
   = Sprite Bullet
            (xCoord sprite, yCoord sprite)
            (0.25, 0.25) 
            Alive
            RedEnemy
            (NoMove, D)
            (0.0)
            (xCoord sprite, yCoord sprite)

mkBullet sprite@(Sprite BlueEnemy _ _ _ _ _ _ _)
   = Sprite Bullet
            (xCoord sprite, yCoord sprite)
            (0.25, 0.25) 
            Alive
            BlueEnemy
            (NoMove, D)
            (0.0)
            (xCoord sprite, yCoord sprite)

mkBullet sprite = sprite



-- makes an input sprite dead

kill :: Sprite -> Sprite
kill (Sprite k l s _ o d c i) = Sprite k l s Dead o d c i



-- euclidean distance between two coordinates

eucDist :: (Float, Float) -> (Float, Float) -> Float
eucDist xy1 xy2 = sqrt $ (fst xy2 - fst xy1)^2 + (snd xy2 - snd xy1)^2


-- predicate true just in case the distance between two sprites
--   is below a threshold (defined in GameConstants.hs)

colliding :: Sprite -> Sprite -> Bool
colliding _ (Sprite HiddnREnmy _ _ _ _ _ _ _) = False
colliding _ (Sprite HiddnBEnmy _ _ _ _ _ _ _) = False
colliding (Sprite HiddnREnmy _ _ _ _ _ _ _) _ = False
colliding (Sprite HiddnBEnmy _ _ _ _ _ _ _) _ = False
colliding spr1 spr2 
  | eucDist (loc spr1) (loc spr2) < collThreshold = True
  | otherwise = False



-- kills all sprites in a list that are colliding with an input 
--   sprite

killColliding :: Sprite -> [Sprite] -> Sprite
killColliding sprite list = if any (\x -> colliding sprite x) list 
                            then kill sprite
                            else sprite


-- kills all bullets and enemies in a state that are colliding 
--   with each other, and augments the score by 1 for each enemy
--   the player is currently killing, and kills the player if
--   an enemy bullet is hitting the player

carnage :: GameState -> GameState
carnage state@(Game _ _ _ _ _ _ _)
   = Game (maybeKillPlayer state) 
          (map (\x -> killColliding x (playerBullets state)) (enemies state))
          ((map (\x -> killColliding x (enemies state)) 
                (playerBullets state) ++ (enemyBullets state)))
          (score state + killCount state)
          (difficulty state)
          (lives state)
          (clock state)
carnage state = state



-- kills all bullets and enemies in a state that are colliding 
--   with each other, and augments the score by 1 for each enemy
--   the player is currently killing, and kills the player if
--   an enemy bullet is hitting the player

enemyCarnage :: GameState -> GameState
enemyCarnage state@(Game _ _ _ _ _ _ _)
   = Game (maybeKPEnemy state) 
          (enemies state) 
          (bullets state)
          (score state)
          (difficulty state)
          (lives state)
          (clock state)
enemyCarnage state = state


-- kills the player when an enemy bullet is colliding with the player

maybeKillPlayer :: GameState -> Sprite
maybeKillPlayer state
   = killColliding (player state) (enemyBullets state)


-- kills the player when an enemy is colliding with the player

maybeKPEnemy :: GameState -> Sprite
maybeKPEnemy state
   = killColliding (player state) (enemies state)

-- given a state, outputs a list of the player's bullets

playerBullets :: GameState -> [Sprite]
playerBullets state
   = filter (ownedBy Player) (bullets state)


-- given a state, outputs a list of the enemy bullets

enemyBullets :: GameState -> [Sprite]
enemyBullets state
   = filter (\x -> ownedBy RedEnemy x || ownedBy BlueEnemy x) (bullets state)


-- true just in case the sprite (i.e. bullet) is owned by the input 
--   character (i.e. red enemy, blue enemy, or player)

ownedBy :: Character -> Sprite -> Bool
ownedBy character sprite 
   = owner sprite == character


-- outputs the number of enemies the player is currently killing

killCount :: GameState -> Int
killCount state
   = sum $ map (\x -> if not (dead x) then 0 else 1) (deadOnes state)
     where
         deadOnes state = map (\x -> killColliding x (bullets state)) 
                              (enemies state)



-- replaces the score of a game state with an input score

swapScore :: Int -> GameState -> GameState
swapScore _score (Game p e b _ d l c) = (Game p e b _score d l c)
swapScore _ state = state

-- replaces the clock of a game state with an input clock

swapClock :: Float -> GameState -> GameState
swapClock _clock (Game p e b s d l _) = (Game p e b s d l _clock)
swapClock _ state = state

-- replaces the clock of a game state with an input clock

swapLives :: Int -> GameState -> GameState
swapLives _lives (Game p e b s d _ c) = (Game p e b s d _lives c)
swapLives _ state = state



-- maps an integer to the initial null game state that corresponds
--   to that level of difficulty

level2Init :: Int -> GameState
level2Init 1 = nullGame
level2Init 2 = nullGame2
level2Init 3 = nullGame3
level2Init 4 = nullGame4
level2Init _ = nullGame4


-- puts an input player into a game state

swapPlayer :: Sprite -> GameState -> GameState
swapPlayer newPlayer (Game _ e b s d l c) = Game newPlayer e b s d l c
swapPlayer _ state = state
