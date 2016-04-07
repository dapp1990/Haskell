-- Return all subsets of a given list
subsets :: [a] -> [[a]]
subsets l = go (length l) l
          where
            go 0 _ = [[]]
            go _ [] = []
            go k (x:xs) = map (x:) (go (k - 1) xs) ++ go (k - 1) xs


main :: IO ()
main = do 
        l <- readLn :: IO Int
        ns <- getLine
        c <- readLn :: IO Int
        cs <- readCases c
        solve l ns c cs
        return ()

readCases :: Int -> IO [Int]
readCases 0 = return []
readCases c = do 
                i <- readLn :: IO Int
                is <- readCases $ c-1
                return $ i:is

string2ints :: Int -> String -> [Int]
string2ints l ns = take l . map read $ words ns :: [Int]

solve :: Int -> String -> Int -> [Int] -> IO ()
solve l ns 0 _ =  return ()
solve l ns c (x:xs) =  do
                        putStrLn $ show $ sizeMinimalSubset (subsets (string2ints l ns)) x
                        solve l ns (c-1) xs

subsets :: [a] -> [[a]]
subsets l = go (length l) l
          where
            go 0 _ = [[]]
            go _ [] = []
            go k (x:xs) = map (x:) (go (k - 1) xs) ++ go (k - 1) xs

sizeMinimalSubset :: [[Int]] -> Int -> Int
sizeMinimalSubset xs v = myMin $ map length [x | x <- xs, sum x >= v]
                       where 
                          myMin [] = -1
                          myMin xs = minimum xs