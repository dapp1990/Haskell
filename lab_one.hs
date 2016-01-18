{-
-- Hors d’Oeuvres
-}
import Data.List

{-
-- myLast
myLast :: [Int] -> Int
myLast [_,last] = last
myLast (_:xs) = myLast xs

-- myRepeat
myRepeat :: Int -> Int -> [Int]
myRepeat n x | n == 0    = []
             | otherwise = x : myRepeat (n-1) x 

-- flatten
flatten ::  [[Int]] -> [Int]
flatten []       = []
flatten ((x):xs) = x ++ flatten xs

-- range
range :: Int -> Int -> [Int]
range min max | min > max = []
              | otherwise = [min] ++ range (min+1) max

-- removeMultiples
removeMultiples :: Int -> [Int] -> [Int]
removeMultiples num [] = []
removeMultiples num (x:xs) | (mod x num) > 0  = [x]++removeMultiples num xs
                           | otherwise        = removeMultiples num xs

removeMultiples2 :: Int -> [Int] -> [Int]
removeMultiples2 n l = filter (\x -> mod x n /= 0) l
-}
{-
-- Folds
-}
{-
--Folds 2.1 - Your own implementation
fold :: (a->a->a) -> a -> [a] -> a
fold f a []     = a
fold f a (x:xs) = f x (fold f a xs)
 
mySum :: [Int] -> Int
mySum (x:xs) = fold (+) 0 (x:xs)

myProduct :: [Int] -> Int
myProduct (x:xs) = fold (*) 1 (x:xs)
 
--Folds 2.2 - Associativity
readInBase :: Int -> [Int] -> Int
readInBase b [] = 0
readInBase b (x:xs) = ((b ^ (length xs)) * x) + readInBase b (xs)

--Folds 2.1 - myMap
myMap :: (a -> b) -> [a] -> [b]
myMap f [] = []
myMap f (x:xs) = f x : (myMap f xs)


readInBase :: Int -> [Int] -> Int
readInBase b = foldl (\x y -> x*b+y) 0

myMap :: (a -> b) -> [a] -> [b]
myMap f = foldr (\x y -> f x : y) []
-}
{-
-- Unpair
-}
{-
unpair :: [(a, b)] -> ([a], [b])
unpair xs = (map fst xs, map snd xs)

unpair2 :: [(a, b)] -> ([a], [b])
unpair2 xs = ([a | (a, _) <- xs], [b | (_, b) <- xs])
-}
{-
-- transpose

transpose :: [[a]] -> [[a]]
transpose ([]:_) = []
transpose xs     = map head xs : transpose (map tail xs)
-}

{-
-- transpose
-}

{-
removeMultiples :: Int -> [Int] -> [Int]
removeMultiples n l = filter (\x -> mod x n /= 0) l
-}
-- Notice the possibility to perform an eta reduction here:
-- removeMultiples n = filter (\x -> mod x n /= 0)
{-

sieve :: Int -> [Int]
sieve n = sieve2 [2..n] n

-- WITHOUT SQUARE ROOT BOUND:
--sieve2 :: [Int] -> Int -> [Int]
--sieve2 []     _ = []
--sieve2 (x:xs) n = x : sieve2 (removeMultiples x xs) n

-- WITH SQUARE ROOT BOUND:
sieve2 :: [Int] -> Int -> [Int]
sieve2 (x:xs) n | x <= floor_square n = x : sieve2 (removeMultiples x xs) n
                | otherwise           = x : xs

sqrtMono :: Double -> Double
sqrtMono = sqrt

i2d :: Int -> Double
i2d = fromIntegral

floorMono :: Double -> Int
floorMono = floor

floorSquare :: Int -> Int
floorSquare n = floorMono (sqrtMono (i2d n))
-- OR: floorSquare = floorMono . sqrtMono . i2d
-}

