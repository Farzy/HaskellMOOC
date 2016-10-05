-- random

import System.Random

dice :: IO ()
dice = do
    roll1 <- getStdRandom (randomR (1::Int,6))
    roll2 <- getStdRandom (randomR (1::Int,6))
    putStrLn ("You rolled " ++ show roll1 ++ " and " ++ show roll2 ++ ".")
