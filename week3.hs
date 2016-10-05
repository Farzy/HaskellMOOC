-- week3.hs

length1 :: [a] -> Int
length1 []     = 0
length1 (x:xs) = 1 + length1 xs

length2 :: Eq a => [a] -> Int
length2 lst = if lst == []
                then 0
                else let (x:xs) = lst in 1 + length2 xs

length3 :: Eq a => [a] -> Int
length3 lst 
  | lst == []  = 0
  | otherwise  = let (x:xs) = lst in 1 + length3 xs

f = f' where f' 1 = 1   ; f' x = x + f' (x-1)

filter1 :: (a -> Bool) -> [a] -> [a]
filter1 pred lst
  | null lst  = []
  | otherwise = if pred x
            then x : filter1 pred xs
            else filter1 pred xs
              where (x:xs) = lst
