module Main where

week' :: Integral a => a -> a -> a
week' y d = let y' = y - 1 in (y' + div y' 4 - div y' 100 + div y' 400 + d) `mod` 7

isLeapYear :: Integral a => a -> Bool
isLeapYear y = mod y 4 == 0  && mod y 100 /= 0 || mod y 400 == 0

monthDays :: (Integral a1, Num a2, Num p, Eq a2) => a1 -> a2 -> p
monthDays y m | m == 2 = if not $ isLeapYear y then 28 else 29
              | m `elem` [1,3,5,7,8,10,12] = 31
              | m `elem` [4,6,9,11] = 30
              | otherwise = error "invalid month"

accDays :: (Integral a1, Num p, Ord p) => a1 -> Int -> p -> p
accDays y m d | d > monthDays y m = error "invalid days"
              | otherwise = sum (take(m-1)(map (monthDays y) [1..12])) + d

week :: Integral a1 => a1 -> Int -> a1 -> a1
week y m d = week' y $ accDays y m d              

main :: IO ()
main = print $ week 2022 09 04