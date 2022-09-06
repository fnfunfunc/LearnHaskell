module Main where

fix :: (t -> t -> Bool) -> (t -> t) -> t -> t
fix cond f x | cond x (f x) = x -- stop!!!!
             | otherwise = fix cond f (f x) -- calculate x by f x

newton :: Fractional a => a -> a -> a
newton c t = (c/t + t) / 2.0

mysqrt :: (Ord a, Fractional a) => a -> a
mysqrt c = fix (\a b -> a - b < 0.000001) (newton c) c


main :: IO ()
main = do
       print $ mysqrt 10