-- week1.hs

import Data.Map

roots a b c =
    let
        det2 = b*b-4*a*c
        det  = sqrt det2
        rootp = (-b + det)/a/2
        rootm = (-b - det)/a/2
    in
        [rootp,rootm]

mymax x y =
    if x > y
        then x
        else y

data Color = Red | Blue | Yellow

set_color = undefined
action1 = undefined
action2 = undefined
action3 = undefined

color = set_color
action = case color of 
            Red -> action1
            Blue -> action2
            Yellow -> action3

set :: Data.Map.Map String Integer 
set = Data.Map.empty

xs = [1,2,xs2 !! 5,4]
xs2 = xs ++ xs
