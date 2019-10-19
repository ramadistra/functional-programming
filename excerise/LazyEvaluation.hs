divisor n = [x | x <- [1..n], n `mod` x == 0]

quicksort [] = []
quicksort (p:xs) = (quicksort [x | x <- xs, x <= p]) ++ [p] ++ (quicksort [x | x <- xs, x > p])

permutation [] = [[]]
permutation xs = [(x:l) | x <- xs, l <- permutation [y | y <- xs, y /= x]]

primes = sieve [2..]
    where sieve (p:xs) = p : sieve [x | x <- xs, x `mod` p /= 0]

pythaTriple = [(x, y, z) | z <- [5..], y <- [1..z], x <- [1..y], (x*x) + (y*y) == (z*z)]

fibs = 1:1:zipWith (+) fibs (tail fibs)
