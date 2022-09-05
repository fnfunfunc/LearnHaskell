module Main where

swaps :: Ord a => [a] -> [a]
swaps [] = []
swaps [x] = [x]
swaps (x1:x2:xs) | x1 > x2 = x2 : swaps(x1:xs)
                 | otherwise = x1 : swaps(x2:xs)

bubbleSort :: Ord a => [a] -> [a]
bubbleSort [] = []
bubbleSort xs = bubbleSort initialElements ++ [lastElement]
    where swappedxs = swaps xs
          initialElements = init swappedxs
          lastElement =last swappedxs

delete :: Eq t => t -> [t] -> [t]
delete _ [] = []
delete n (x:xs) | n == x = xs
                | otherwise = x : delete n xs

selectionSort :: Ord a => [a] -> [a]
selectionSort [] = []
selectionSort xs = mini : selectionSort xs'
    where mini = minimum xs
          xs' = delete mini xs

filterSplit :: (a -> Bool) -> [a] -> ([a], [a])
filterSplit _ [] = ([], [])
filterSplit f (x:xs) | f x = (x:l, r)
                     | otherwise = (l, x:r)
                     where (l,r) = filterSplit f xs

quickSort :: Ord a => [a] -> [a]
quickSort [] = []
quickSort (x:xs) = quickSort mini ++ [x] ++ quickSort maxi
    where (mini, maxi) = filterSplit (<x) xs

merge :: Ord a => [a] -> [a] -> [a]
merge xs [] = xs
merge [] ys = ys
merge (x:xs) (y:ys) | x > y = y:merge (x:xs) ys
                    | otherwise = x:merge xs (y:ys)


mergeSort :: Ord a => [a] -> [a]
mergeSort xs = merge (mergeSort x1) (mergeSort x2)
    where (x1, x2) = halve xs
          halve xs = splitAt l xs
          l = length xs `div` 2

main :: IO ()
main = do
    print $ bubbleSort [23,121,231,11,2,34,656,234,211]
    print $ selectionSort [23,121,231,11,2,34,656,234,211]
    print $ quickSort [23,121,231,11,2,34,656,234,211]
    print $ mergeSort [23,121,231,11,2,34,656,234,211]
    