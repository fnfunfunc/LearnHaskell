module Main where


romeNotation :: [String]
romeNotation = ["M", "CM", "D", "CD", "C", "XC", "L", "XL", "X", "IX", "V", "IV", "I"]

romeAmount :: [Integer]
romeAmount = [1000, 900, 500, 400, 100, 90, 50, 40, 10, 9, 5, 4, 1]

romePair :: [(Integer, String)]
romePair = zip romeAmount romeNotation

subtrahend :: Integer -> (Integer, String)
subtrahend n = head $ dropWhile (\(a, _) -> a > n) romePair

convert :: Integer -> [Char]
convert 0 = ""
convert n = let (rome, m) = subtrahend n
                in m ++ convert(n - rome)

main :: IO ()
main = do
    print $ convert 4