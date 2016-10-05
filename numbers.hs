-- numbers.hs
-- Author: Farzad FARID
-- Haskell MOOC lesson 2.11

import System.Random

turn :: Int -> Int -> Int -> IO ()
turn num guess tries
    | tries == 0   = putStrLn "You lose!" 
    | num == guess = putStrLn "You win!"
    | otherwise    = do
        putStrLn ("Too " ++ (if num < guess then "high" else "low") ++ "...")
        mkguess num tries 

mkguess :: Int -> Int -> IO ()
mkguess num tries =
    do
        putStr ("Type a number [" ++ (take tries (repeat '*')) ++ "] -> ")
        a <- getLine
        let guess' = read a :: Int
        let tries' = if num == guess'
                    then tries
                    else tries - 1
        turn num guess' tries'

numbers :: IO ()
numbers = 
    do
        number <- randomRIO (1, 20) :: IO Int
        mkguess number 5

