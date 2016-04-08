-- Link https://www.hackerrank.com/challenges/super-queens-on-a-chessboard

main :: IO ()
main = do 
        n <- readLn :: IO Int
        putStrLn . show . length $ superQueen n

superQueen n = generate n
              where generate 0 = [[]]
                    generate k = [q : qs | qs <- generate (k-1), q <- [1..n], isSafe q qs]
                    isSafe try qs = not (try `elem` qs || sameDiag try qs || hourseMovement try qs 2)
                    sameDiag try qs = any (\(colDist,q) -> abs (try - q) == colDist) $ zip [1..] qs
                    hourseMovement _ [] _ = False
                    hourseMovement try (q:qs) 1 = ( (try+1) == q || (try-1) == q)
                    hourseMovement try (q:qs) 2 = ( (try+2) == q || (try-2) == q || hourseMovement try qs 1)
