-- Link https://www.hackerrank.com/challenges/string-reductions
-- Return all subsets of a given list
main :: IO ()
main = do 
        s <- getLine
        putStrLn $ stringReduction s

stringReduction   :: String -> String
stringReduction [] = []
stringReduction (x:xs) = (++) [x] (stringReduction (remove x xs))

remove :: Char -> String -> String
remove y []     = []
remove y (x:xs) | y == x    = remove y xs
                | otherwise = [x] ++ remove y (xs)