module Main where

factors :: Integral a => a -> [a]
factors n = [x | x <- [1..n], mod n x == 0]

isPrime :: Integral a => a -> Bool
isPrime 2 = True
isPrime p =
    p > 1 && all (\n -> p `mod` n /= 0) (takeWhile (\n -> n * n <= p) [3, 5 ..])


-- Eratosthenes sieve （埃拉托斯特尼筛法）
sieve :: Integral a => [a] -> [a]
sieve [] = []
sieve (p:xs) = p : sieve [x | x <- xs, x `mod` p /= 0]

prime :: [Integer]
prime = sieve [2..]


-- Get the times
getExp :: (Integral t, Num p) => t -> t -> p
getExp n k | mod n k /= 0 = 0
           | otherwise = 1 + getExp (div n k) k

-- Get the pair
getPair :: (Eq a, Integral t, Num a) => t -> t -> [(a, t)]
getPair n k | t == 0 = []
      | otherwise = [(t, k)]
      where t = getExp n k

-- Prime factorization （质因数分解）
primeFactors :: (Eq a, Num a) => Integer -> [(a, Integer)]
primeFactors n | n < 2 = error ("Can't get prime factors of " ++ show n)
               | otherwise = concatMap (getPair n) $ takeWhile (<=n) prime     

main :: IO ()
main = do
    print $ primeFactors 114514
    