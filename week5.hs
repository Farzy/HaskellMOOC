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
