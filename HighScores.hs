{- 
   File      : HighScores.hs
   Copyright : (c) Matt Teichman, 3/15/17 

   Module for handling the high scores in Galaga.hs.

-}


module HighScores where
import Text.Read
import Data.List
import Graphics.Gloss
import Graphics.Gloss.Data.Picture



-- reads the high scores file in, makes it a list, 
--   adds the input score to the list

scoreList :: Int -> IO [Int]
scoreList newScore = do 
                _string <- readFile "highScores.txt"
                return $ take 10 
                       $ reverse
                       $ sort 
                       $ ((:) newScore)
                       $ map readScore 
                       $ lines _string

-- displays a message indicating that you're in the high scores screen

hiScoresMessage :: IO Picture
hiScoresMessage 
   = return $ color white                       
            $ translate (-350) (75)
            $ scale 0.25 0.25
            $ text ("These are the high scores: ")


-- displays a message: "Press any key to continue..."

pressAnyKey :: IO Picture
pressAnyKey
   = return $ color white                       
            $ translate (-350) (-75)
            $ scale 0.25 0.25
            $ text ("Press any key to continue...")


-- displays the high scores

hiScoresPic :: Int -> IO Picture
hiScoresPic score 
   = do 
       hiScores <- scoreString score
       return $ color white                       
              $ translate (-350) (0)
              $ scale 0.25 0.25
              $ text (hiScores)

-- converts a list of ints to an IO list of strings

stringify :: [Int] -> IO [String]
stringify _list
   = return $ map show _list


-- converts an IO list of strings into an IO string concatenating them

showScores :: IO [String] -> IO String
showScores ioStrList
   = do
        _string <- ioStrList
        return $ foldr1 (\x y -> x ++", "++ y) _string



-- reads the high scores file, appends an input score
--   to the resulting list, converts said list into a string
--   so that it can be displayed

scoreString newScore 
   = do
       _list <- scoreList newScore
       showScores $ stringify _list


-- reads a string and converts it to an int

readScore :: String -> Int
readScore string 
   = case readMaybe string :: Maybe Int of
     Just n -> n
     Nothing -> error "poorly formatted high scores file"
               

-- writes the high score back to the file

writeScore :: Int -> IO ()
writeScore newScore
   = appendFile "highScores.txt" (show newScore ++ "\n")

-- converts the updated high score int list into a string to be 
--   written to a file

generateTextFile :: IO [Int] -> IO String
generateTextFile ioIntList
   = do
        intScores <- ioIntList
        let scores = map show intScores
        return $ foldr1 (\x y -> x ++"\n"++ y) scores

