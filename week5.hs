-- week5.hs

fibs = 1:1:(zipWith (+) fibs (tail fibs))

properfactors :: Int -> [Int]
properfactors x = filter (\y->(x `mod` y == 0)) [2..(div x 2)]

numproperfactors :: Int -> Int
numproperfactors x = length (properfactors x)

primes :: [Int]
primes = filter (\x-> (numproperfactors x == 0)) [2..]

-- From https://github.com/jculpin/Tutorial/blob/tutorial-branch/perfect.hs
-- A perfect number is a number for which the sum of its proper divisors is exactly equal to the 
-- number. For example, the sum of the proper divisors of 28 would be 1 + 2 + 4 + 7 + 14 = 28, 
-- which means that 28 is a perfect number.

sumproperfactors :: Int -> Int
sumproperfactors = (1+) . sum . properfactors

perfect :: Int -> Bool
perfect x = sumproperfactors x == x

listPerfect = filter perfect [2..]

-----
-- William Morgan
triples = [ (a,b,c) | c <- [1..], b <- up c, cp b c, a <- up b, pt a b c ]
    where
        up n = [1..(n - 1)]
        pt a b c = a^2 + b^2 == c^2
        cp a b = if b == 0 then a == 1 else cp b (a `mod` b)
