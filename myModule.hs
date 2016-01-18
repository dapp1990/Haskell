-- Return all subsets of a given list
subsets :: [a] -> [[a]]
subsets l = go (length l) l
          where
            go 0 _ = [[]]
            go _ [] = []
            go k (x:xs) = map (x:) (go (k - 1) xs) ++ go (k - 1) xs