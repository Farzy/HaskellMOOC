-- ffcount.hs

sub :: [a] -> [[a]]
sub []     = [[]]
sub (x:xs) = ys ++ map (x:) ys
             where
                ys = sub xs

interleave :: a -> [a] -> [[a]]
interleave a []     = [[a]]
interleave a (x:xs) = (a:(x:xs)) : (map (x:) (interleave a xs))

perms :: [a] -> [[a]]
perms []     = [[]]
perms (x:xs) = concat $ map (interleave x) (perms xs)

split :: [a] -> [([a],[a])]
split [] = [([],[])]
split (x:xs) = ([x],xs) : map (\t -> (x : fst t, snd t)) (split xs)

ne :: ([a],[b]) -> Bool
ne (xs,ys) = not (null xs || null ys)

nesplit :: [a] -> [([a],[a])]
nesplit = filter ne . split
