{- 
   File      : PlayerMoves.hs
   Copyright : (c) Matt Teichman, 3/15/17 

   Functions for moving the player in Galaga.hs.

-}
 

module Sprite where  

import Graphics.Gloss
import GameConstants

-- five directions for sprites to point in: left, right, up, down, and nowhere

data Direction = L | R | U | D | NoMove
   deriving (Show, Eq)


-- state of a ship's health

data HealthState = Alive | Dead | Exploding | Attacking
   deriving (Show, Eq)

-- kinds of sprite:
--
--   player
--   red enemy
--   blue enemy
--   red enemy
--   bullet
--   no one
--   a hidden red enemy
--   a hidden blue enemy

data Character = Player 
               | RedEnemy 
               | BlueEnemy 
               | HiddnREnmy 
               | HiddnBEnmy 
               | Bullet 
               | NoOne
   deriving (Show, Eq)


-- Sprite datatype, which includes information about what kind 
--   of sprite it is, where it is, how big it is, whether it's alive,
--   what its owner is (only relevant for bullets), what direction
--   it's pointing in, an internal clock, and the position it started in.

--     kind - what kind of sprite
--     loc - location of sprite
--     size - size of sprite
--     health - health status of sprite
--     owner - owner of sprite (only relevant for bullets)
--     dir - direction sprite is pointed in
--     sClock - internal clock on the sprite
--     initPos - location where sprite started

data Sprite = Sprite {kind :: Character,
                      loc :: (Float, Float),
                      size :: (Float, Float),
                      health :: HealthState,
                      owner :: Character,
                      dir :: (Direction, Direction),
                      sClock :: Float,
                      initPos :: (Float, Float)}
    deriving (Show, Eq)

-- function for pulling x coordinate out of a sprite

xCoord :: Sprite -> Float
xCoord sprite = fst $ loc sprite


-- function for pulling y coordinate out of a sprite

yCoord :: Sprite -> Float
yCoord sprite = snd $ loc sprite


-- function for pulling the x size out of a sprite

xSize :: Sprite -> Float
xSize sprite = fst $ size sprite


-- function for pulling the y size out of a sprite

ySize :: Sprite -> Float
ySize sprite = snd $ size sprite


-- function for pulling the starting x coordinate out of sprite

xIPos :: Sprite -> Float
xIPos sprite = fst $ initPos sprite


-- function for pulling the starting y coordinate out of a sprite

yIPos :: Sprite -> Float
yIPos sprite = snd $ initPos sprite


-- function for rendering a sprite (i.e. making it into a Picture)

render :: Sprite -> IO Picture 
render sprite@(Sprite Player _ _ _ _ _ _ _) = do 
    pic <- shipPic
    return $ (transNScale sprite) pic

-- hidden enemies are invisible

render sprite@(Sprite HiddnREnmy _ _ _ _ _ _ _) = return Blank 
render sprite@(Sprite HiddnBEnmy _ _ _ _ _ _ _) = return Blank 

render sprite@(Sprite BlueEnemy _ _ _ _ _ _ _) = do
    pic <- bEnemyPic
    return $ (transNScale sprite) pic

render sprite@(Sprite RedEnemy _ _ _ _ _ _ _) = do
    pic <- rEnemyPic
    return $ (transNScale sprite) pic

render sprite@(Sprite Bullet _ _ _ Player _ _ _) = do
    pic <- pBullPic
    return $ (transNScale sprite) pic

render sprite@(Sprite Bullet _ _ _ RedEnemy _ _ _) = do
    pic <- eBullPic
    return $ (transNScale sprite) pic

render sprite@(Sprite Bullet _ _ _ BlueEnemy _ _ _) = do
    pic <- eBullPic
    return $ (transNScale sprite) pic

render _ = error "sprite not renderable"


-- helper functions for rendering sprites

-- translates a sprite to its x coordinate and y coordinate

sprTrans sprite = translate (xCoord sprite) (yCoord sprite)


-- scales a sprite to its x size and y size

sprScale sprite = scale (xSize sprite) (ySize sprite)


-- translates and scales a sprite

transNScale sprite = sprTrans sprite . sprScale sprite 


-- converts Directions to a multiplier that will determine
--   how to translate directions into transformations on x and y
--   coordinates

dirToFloat :: Direction -> Float
dirToFloat L = (-1.0)
dirToFloat R = (1.0)
dirToFloat U = (1.0)
dirToFloat D = (-1.0)
dirToFloat NoMove = (0.0)


-- moves a sprite by a tiny amount in whatever direction it's pointing
--   first parameter is degree of horizontal movement
--   second parameter is degree of vertical movement

nudge :: Float -> Float -> Sprite -> Sprite
nudge inc1 inc2 sprite 
  = Sprite (kind sprite)
           (xCoord sprite + (inc1 * dirToFloat (fst $ dir sprite)), 
            yCoord sprite + (inc2 * dirToFloat (snd $ dir sprite)))
           (size sprite)
           (health sprite)
           (owner sprite)
           (dir sprite)
           (sClock sprite)
           (initPos sprite)


-- changes direction of a sprite

aboutFace :: Sprite -> Sprite
aboutFace sprite 
   = Sprite (kind sprite)
            (loc sprite)
            (size sprite)
            (health sprite)
            (owner sprite)
            (oppDir $ fst $ dir sprite, oppDir $ snd $ dir sprite)
            (sClock sprite)
            (initPos sprite)


-- maps a direction to its reverse
 
oppDir :: Direction -> Direction
oppDir L = R
oppDir R = L
oppDir U = D
oppDir D = U
oppDir NoMove = NoMove


-- bitmap picture for blue enemy

bEnemyPic :: IO Picture
bEnemyPic = loadBMP "images/models/blue_fighter.bmp"


-- bitmap picture for red enemy

rEnemyPic :: IO Picture
rEnemyPic = loadBMP "images/models/red_fighter.bmp"


-- bitmap picture for player ship

shipPic :: IO Picture  
shipPic = loadBMP "images/models/ship.bmp"



-- bitmap picture for player bullet

pBullPic :: IO Picture
pBullPic = loadBMP "images/models/ship_bullet.bmp"


-- bitmap picture for enemy bullet

eBullPic :: IO Picture
eBullPic = loadBMP "images/models/enemy_bullet.bmp"



-- helper functions for ioPictures

-- lifts a two-place relation to a two-place relation on applicatives

ioApplication :: Applicative f => (a -> b -> c) -> f a -> f b -> f c
ioApplication = \f x y -> ((f) <$> x <*> y)


-- groups two pictures into a single pictures

groupPic :: Picture -> Picture -> Picture
groupPic pic1 pic2 = pictures [pic1, pic2]


-- folds a two-place relation over an applicative, within the applicative,
--   then puts the applicative constructor back around the result

unwrapIO :: (Foldable t, Applicative f) => (a -> a -> a) -> t (f a) -> f a
unwrapIO f list = foldr1 (ioApplication f) list


-- analogue of Gloss' pictures function, but which groups together IO Pictures
-- into a single IO Picture

ioPictures :: [IO Picture] -> IO Picture
ioPictures [] = return Blank
ioPictures [pic] = pic
ioPictures picList = unwrapIO groupPic picList


-- true of a sprite just in case it is hidden

hidden :: Sprite -> Bool
hidden (Sprite HiddnBEnmy _ _ _ _ _ _ _ ) = True
hidden (Sprite HiddnREnmy _ _ _ _ _ _ _ ) = True
hidden _ = False



-- true of a sprite just in case it is neither hidden
--   nor attacking

hideable :: Sprite -> Bool
hideable sprite = not (hidden sprite || attacking sprite)


-- predicate true of alive sprites

alive :: Sprite -> Bool
alive (Sprite _ _ _ Alive _ _ _ _) = True
alive _ = False



-- predicate true of dead sprites

dead :: Sprite -> Bool
dead (Sprite _ _ _ Dead _ _ _ _) = True
dead _ = False



-- predicate true of an attacking enemy

attacking :: Sprite -> Bool
attacking (Sprite _ _ _ Attacking _ _ _ _) = True
attacking _ = False



-- predicate indicating that a sprite has gone offscreen

outOfBounds :: Sprite -> Bool
outOfBounds sprite 
   = xCoord sprite > (fromIntegral $ scrWidth)/2 + pad
     || xCoord sprite < ((fromIntegral $ scrWidth)/2 * (-1)) - pad
     || yCoord sprite > (fromIntegral $ scrHeight)/2 + pad
     || yCoord sprite < ((fromIntegral $ scrHeight)/2 * (-1)) - pad


-- initial state of the player's ship.

playerShip :: Sprite
playerShip = Sprite Player 
                    (0.0, -235.0) 
                    (0.25, 0.25) 
                    Alive 
                    NoOne 
                    (NoMove, NoMove) 
                    0.0 
                    (0.0, -235.0)



-- creates a list of coordinates for creating a row of enemies
                  
rowInitialize :: (Num a, Enum a) => a -> a -> [(a,a)]
rowInitialize width offset = zip ((map (* (-1)) (reverse [50,100..width]) 
                                                 ++ [0,50..width]))
                                  (replicate 12 offset)

