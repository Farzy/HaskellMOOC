-- week2.hs

import Data.Bits

a = [(x,y) | x <- [False,True], y <- [False,True]]
-- [(False,False),(False,True),(True,False),(True,True)]

b = map (\inputs -> xor (fst inputs) (snd inputs)) [(x,y) | x <- [False,True], y <- [False,True]]
-- [False,True,True,False]

-- do { putStrLn "what is your name?"; x <- getLine; putStrLn ("hello " ++ x) }
-- do { putStrLn "what is your name?"; x <- getLine; let nUpper = map toUpper x in putStrLn ("HELLO " ++ nUpper) }

greet :: IO ()
greet  = 
    do planet <- getLine
       home <- getLine
       putStrLn ("greetings " ++ planet ++ "ling.")
       putStrLn ("I am from " ++ home ++ ".")
       putStrLn "Take me to your leader."


mknoble :: Bool -> String -> String
mknoble female name = (if female then "Dame " else "Sir ") 
                            ++ name
