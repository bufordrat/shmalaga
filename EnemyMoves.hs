{- 
   File      : EnemyMoves.hs
   Copyright : (c) Matt Teichman, 3/15/17 

   Functions for use in Galaga.hs that manipulate the enemies.
   
   (Note that enemies, bullets, and the player are all of type Sprite.)   

-}

module EnemyMoves where

import GameConstants
import GameState
import Choreography
import Sprite
import Shuffle
import Data.List
import Data.Fixed
import Debug.Trace


-- moves an enemy by a tiny increment in the direction it's facing
--  and changes direction if it's a certain distance from its starting
--  location
 
moveEnemy :: Float -> Float -> Sprite -> Sprite
moveEnemy deltaTime enemySpeed sprite 
   | attacking sprite = nudge (deltaTime * enemySpeed) 
                              (deltaTime * enemySpeed) 
                              sprite
moveEnemy deltaTime enemySpeed sprite@(Sprite RedEnemy _ _ _ _ _ _ _) 
   | abs (xCoord sprite - xIPos sprite) < 101 
     = nudge (deltaTime * enemySpeed) 0.0 sprite
   | otherwise 
     = nudge (deltaTime * enemySpeed) 0.0 (aboutFace sprite)
moveEnemy deltaTime enemySpeed sprite@(Sprite BlueEnemy _ _ _ _ _ _ _) 
   | abs (xCoord sprite - xIPos sprite) < 101 
     = nudge (deltaTime * enemySpeed) 0.0 sprite
   | otherwise 
     = nudge (deltaTime * enemySpeed) 0.0 (aboutFace sprite)
moveEnemy deltaTime enemySpeed sprite@(Sprite HiddnREnmy _ _ _ _ _ _ _) 
   | abs (xCoord sprite - xIPos sprite) < 101 
     = nudge (deltaTime * enemySpeed) 0.0 sprite
   | otherwise 
     = nudge (deltaTime * enemySpeed) 0.0 (aboutFace sprite)
moveEnemy deltaTime enemySpeed sprite@(Sprite HiddnBEnmy _ _ _ _ _ _ _) 
   | abs (xCoord sprite - xIPos sprite) < 101 
     = nudge (deltaTime * enemySpeed) 0.0 sprite
   | otherwise 
     = nudge (deltaTime * enemySpeed) 0.0 (aboutFace sprite)
moveEnemy _ _ sprite = sprite

 
-- moves all enemies in a state by a tiny increment in the direction 
--   they're facing 

moveEnmyState :: Float -> Float -> GameState -> GameState
moveEnmyState deltaTime enemySpeed state@(Game _ _ _ _ _ _ _) 
   = Game (player state) 
          (map (\x -> moveEnemy deltaTime enemySpeed x) 
               (enemies state))
          (bullets state)
          (score state)
          (difficulty state)
          (lives state)
          (clock state)
moveEnmyState _ _ state = state




-- creates a new attacking enemy in the current location of
--   an input sprite

mkAttacker :: Sprite -> Sprite
mkAttacker sprite@(Sprite BlueEnemy _ _ _ _ _ _ _)
   = Sprite BlueEnemy 
            (loc sprite)
            (size sprite)
            Attacking
            (owner sprite)
            (NoMove, D)
            (sClock sprite)
            (loc sprite)
mkAttacker sprite@(Sprite RedEnemy _ _ _ _ _ _ _)
   = Sprite RedEnemy 
            (loc sprite)
            (size sprite)
            Attacking
            (owner sprite)
            (NoMove, D)
            (sClock sprite)
            (loc sprite)
mkAttacker _ = error "mkAttacker must be called on an enemy" 



-- true just in case two sprites have the same starting location

sharedHome :: Sprite -> [Sprite] -> Bool
sharedHome sprite list
   = any (\x -> sameHome x sprite) list



-- advances the internal clock on a sprite by a tiny amount

advSClock :: Float -> Sprite -> Sprite
advSClock deltaTime (Sprite k l s h o d c i) 
   = Sprite k l s h o d (c + deltaTime) i


-- resets the internal clock on a sprite

resetClock :: Sprite -> Sprite
resetClock (Sprite k l s h o d c i)
   = Sprite k l s h o d (0.0) i


-- chooses a random (non-hidden) enemy from a list, creates an 
--   attacker in its location, and conses that attacker
--   onto the list

enemiesAttack :: (Sprite -> Bool) -> [Sprite] -> IO [Sprite]
enemiesAttack f list 
   = let (a,b) = partition f list 
     in let a' = shuffle a 
        in do
           list <- a'
           if list == [] 
           then error "no hideable enemies" -- change this to return [] once multiple levels work
           else return $ ((mkAttacker (head list)):
                          (hideEnemy (head list)):
                          ((tail list)
                          ++b))


-- hides an enemy sprite

hideEnemy :: Sprite -> Sprite
hideEnemy (Sprite RedEnemy l s h o d c i) 
   = Sprite HiddnREnmy l s h o d c i
hideEnemy (Sprite BlueEnemy l s h o d c i) 
   = Sprite HiddnBEnmy l s h o d c i
hideEnemy x = error ("hideEnemy should only be called on enemies; "++
                     "was called on "++show x)


-- unhides an enemy sprite

unhideEnemy :: Sprite -> Sprite
unhideEnemy (Sprite HiddnREnmy l s h o d c i) 
   = Sprite RedEnemy l s h o d c i
unhideEnemy (Sprite HiddnBEnmy l s h o d c i) 
   = Sprite BlueEnemy l s h o d c i
unhideEnemy x = x



-- updates a state to one in which a random enemy is now attacking
--   and there is a hidden enemy where it used to be in the formation

attack :: GameState -> IO GameState
attack state@(Game _ _ _ _ _ _ _)
   | any hideable (enemies state)  
      = do
         newEnemies <- enemiesAttack hideable (enemies state)
         return $ Game (player state)
                       (newEnemies)
                       (bullets state)
                       (score state)
                       (difficulty state)
                       (lives state)
                       (clock state)
   | otherwise = return $ state
attack state = return $ state

     

-- if the specified interval of time has elapsed, run attack
--   on the game state; otherwise, do nothing

timeToAttack :: Float -> GameState -> IO GameState
timeToAttack deltaTime state@(Game _ _ _ _ _ _ _)
   | mod' (clock state) (attackInterval / 
                         (fromIntegral (difficulty state))/2) 
                        < (deltaTime) 
     && mod' (clock state) (attackInterval / 
                            (fromIntegral (difficulty state))/2) 
                           >= (deltaTime) * (-1)
     && (clock state) > 0
     = attack state
   | otherwise = return state
timeToAttack _ state = return state



-- if the specified interval of time has elapsed, have 
--   all attackers fire

timeToFire :: Float -> GameState -> IO GameState
timeToFire deltaTime state@(Game _ _ _ _ _ _ _)
   | mod' (clock state) ((attackInterval/1.5) / 
                         (fromIntegral (difficulty state))/2) 
                        < (deltaTime) 
     && mod' (clock state) ((attackInterval/1.5) / 
                            (fromIntegral (difficulty state))/2) 
                           >= (deltaTime) * (-1)
     && (clock state) > 0
     = attackersFire state
   | otherwise = return state
timeToFire _ state = return state


-- halts a sprite in its current location

halt :: Sprite -> Sprite
halt (Sprite k l s h o _ c i) = Sprite k l s h o (NoMove, NoMove) c i


-- advances the clock on an attacking sprite

advClock :: Float -> Sprite -> Sprite
advClock deltaTime (Sprite k l s h o d c i)
   = (Sprite k l s h o d (c + deltaTime) i)


-- advances the clock on all attacking sprites within a state

advClockAttackers :: Float -> GameState -> GameState
advClockAttackers deltaTime (Game p e b s d l c)
   = Game p
          (modifyPred attacking (advClock deltaTime) e)
          b
          s
          d
          l
          c
advClockAttackers _ state = state

-- makes all the attacking enemies fire a bullet

attackersFire :: GameState -> IO GameState
attackersFire state@(Game p e b s d l c)
   = return $ Game p
                   e
                   ([mkBullet y | y <- e, attacking y]++b)
                   s
                   d
                   l
                   c 
attackersFire state = return state

xMkBullet y = trace "enemy bullet fired" $ mkBullet y

-- pulls everything that satisfies a predicate out of a list,
--   maps a function over those satisfied things, then appends
--   them back onto the list

modifyPred p f list 
   = let (a,b) = partition p list
     in let c = map f a
        in c ++ b


