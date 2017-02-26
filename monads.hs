-- monads.hs

myHead :: [a] -> Maybe a
myHead []     = Nothing
myHead (x:xs) = Just x

myTail :: [a] -> Maybe [a]
myTail []     = Nothing
myTail (x:xs) = Just xs

bar :: [a] -> Maybe a
bar xs = myTail xs >>= \a ->
         myTail a >>= \b ->
         myHead b

bar' :: [a] -> Maybe a
bar' xs = do
    a <- myTail xs
    b <- myTail a
    myHead b
