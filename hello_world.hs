{-
sumABC :: [(String, Int)] -> Maybe Int
sumABC xs =  case xs of [] -> Nothing
                        xs -> fmap sum $ sequence ((lookup "A" xs) : (lookup "B" xs) : (lookup "C" xs) : [])

sumABCBind :: [(String, Int)] -> Maybe Int
sumABCBind xs = xs >>= \x -> fmap sum $ sequence ((lookup "A" x) : (lookup "B" x) : (lookup "C" x) : [])
-}


mySum = \x -> \y -> \z -> x+y+z
{-
sumABC [(a,b)] = 
    case lookup a [(a,b)] of   
        Nothing -> Nothing
        Just b -> Just b
-}