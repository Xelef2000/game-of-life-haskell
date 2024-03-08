module Main where
import Graphics.Gloss

newtype Game = Game [[Bool]]
  deriving (Show)

squareSize :: Float
squareSize = 20

newGame :: Game
newGame = Game [
    [False, False, False, False, False, False, False, False, False, False],
    [False, True, False, False, False, False, False, False, False, False],
    [False, False, True, False, False, False, False, False, False, False],
    [False, True, True, False, False, False, False, False, False, False],
    [False, False, False, False, False, False, False, False, False, False],
    [False, False, False, False, True, True, True, False, False, False],
    [False, False, False, False, True, True, True, False, False, False],
    [False, False, False, False, True, True, True, False, False, False],
    [False, False, False, False, False, False, False, False, False, False],
    [False, False, False, False, False, False, False, False, False, False]]



displayRow :: [Bool] -> Picture
displayRow row = pictures $ zipWith cellPicture row [0..]
  where
    cellPicture cell i = translate (fromIntegral i * squareSize) 0 (displayCell cell)



displayCell :: Bool-> Picture
displayCell True = color white $ rectangleSolid squareSize squareSize
displayCell False = color black $ rectangleSolid squareSize squareSize


displayGame :: Game -> Picture
displayGame (Game game) = pictures $ zipWith rowPicture game [0..]
  where
    rowPicture row i = translate 0 (-fromIntegral i * squareSize) (displayRow row)


addRowIndicies :: Int -> [Bool] -> [(Bool, (Int , Int))]
addRowIndicies ri = zipWith (\i e -> (e, (ri, i))) [0..]

addGameIndicies :: Game -> [[(Bool, (Int, Int))]]
addGameIndicies (Game game) = zipWith addRowIndicies [0..] game

evalCell :: (Bool, (Int, Int)) -> [[(Bool, (Int, Int))]] -> Bool
evalCell (True, (ri, ci)) game = length (filter (\(b, (i, j)) -> b && (abs (i - ri) <= 1) && (abs (j - ci) <= 1)) (concat game)) `elem` [3, 4]
evalCell (False, (ri, ci)) game = length (filter (\(b, (i, j)) -> b && (abs (i - ri) <= 1) && (abs (j - ci) <= 1)) (concat game)) == 3

evalRow :: [[(Bool, (Int, Int))]] -> [(Bool, (Int, Int))] -> [Bool]
evalRow game = map (`evalCell` game)

avalGame :: [[(Bool, (Int, Int))]] -> [[Bool]]
avalGame game = map (evalRow game) game




nextStep :: Game -> Game
nextStep (Game game) = Game $ avalGame (addGameIndicies (Game game))

main :: IO ()
-- main = display (InWindow "Game of Life" (400, 400) (10, 10)) blue (displayGame newGame)
main = simulate (InWindow "Game of Life" (400, 400) (10, 10)) white 1 newGame displayGame (\_ _ game -> nextStep game)
-- main = do
--   print $ (addGameIndicies newGame)  !! 2 !! 1

