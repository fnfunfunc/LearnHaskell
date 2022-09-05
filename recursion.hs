module Main where

gcd' :: Integral t => t -> t -> t
gcd' x y = if y == 0 then x else gcd' y $ mod x y

power' :: (Eq p, Num p, Integral a) => p -> a -> p
power' 0 0 = 1
power' _ 0 = 1
power' x n | odd n = let p = power' x (div (n-1) 2) in x * p * p
          | otherwise = let p = power' x (div n 2) in p * p

product' :: Num p => [p] -> p
product' [] = 1
product' (x:xs) = x * product' xs

snoc' :: t -> [t] -> [t]
snoc' x [] = [x]
snoc' x (y:ys) = y: snoc' x ys

last' :: [p] -> p
last' [] = error "Empty list"
last' [x] = x
last' (_:xs) = last' xs

take' :: (Ord t, Num t) => t -> [a] -> [a]
take' n _ | n <= 0 = []
take' _ [] = []
take' n (x:xs) = x : take' (n-1) xs

drop' :: (Ord t, Num t) => t -> [a] -> [a]
drop' n xs | n <= 0 = xs
drop' n (x:xs) = drop' (n-1) xs
drop' n [] = []

elem' :: Eq t => t -> [t] -> Bool
elem' _ [] = False
elem' n (x:xs) = (x == n) || elem' n xs

delete' :: Eq a => a -> [a] -> [a]
delete' n [] = []
delete' n (x:xs) = if n == x then delete' n xs else x : delete' n xs
 
main :: IO ()
main = do
    print $ gcd' 12 4
    print $ power' 2 9
    print $ product' [5,2,3,4]
    print $ snoc' 5 [1,2,3,4]
    print $ last' [1,2,4,3]
    print $ take' 4 [1,2,3,4,5,6,7]
    print $ drop' 3 [1,2,3,4,5,6,7]
    print $ delete' 2 [4,2,4,6,2]
    