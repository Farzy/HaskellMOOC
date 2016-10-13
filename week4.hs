-- week4.hs

-- let & where
journeycost :: Float -> Float -> Float
journeycost miles fuelcostperlitre = 
 let milespergallon = 35
     litrespergallon = 4.55
     gallons = miles/milespergallon
 in (gallons*litrespergallon*fuelcostperlitre)

circle radius =
    let diameter = 2*radius
        circumference = pi*diameter
    in (diameter, circumference)

squareplusone :: Int -> Int
squareplusone x = xsquared + 1
 where xsquared = x*x

cel2fahr :: Float -> Float
cel2fahr x = (x*scalingfactor) + freezingpoint
 where scalingfactor = 9.0/5.0
       freezingpoint = 32

-- Guards
holeScore :: Int -> Int -> String
holeScore strokes par
  | strokes < par = show (par-strokes) ++ " under par"
  | strokes == par = "level par"
  | strokes > par = show(strokes-par) ++ " over par"

holeScore' :: Int -> Int -> String
holeScore' strokes par
  | score < 0 = show (abs score) ++ " under par"
  | score == 0 = "level par"
  | otherwise = show(score) ++ " over par"
 where score = strokes-par

-- data Pet = Cat | Dog | Fish
data Pet = Cat | Dog | Fish | Parrot String

hello :: Pet -> String
hello x = 
  case x of
    Cat -> "meeow"
    Dog -> "woof"
    Fish -> "bubble"
    Parrot name -> "pretty " ++ name

hello' :: Pet -> String
hello' x =
  case x of
    Parrot name -> "pretty " ++ name
    _ -> "grunt"

-- Good: let x = numeral ++ " minister" where numeral = "prime" in x
-- Bad:  let x = numeral ++ " minister" in x where numeral = "prime"

maxhelper :: Int -> [Int] -> Int
maxhelper x [] = x
maxhelper x (y:ys) = maxhelper
        (if x < y then y else x) ys

maxfromlist :: [Int] -> Maybe Int
maxfromlist [] = Nothing
maxfromlist (x:xs) = Just (maxhelper x xs)

helloMonad :: String -> IO String
helloMonad x = do
    putStrLn ("Hello " ++ x)
    putStrLn "What is your name?"
    name <- getLine
    return name

