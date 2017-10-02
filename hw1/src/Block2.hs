{-# LANGUAGE ScopedTypeVariables #-}

module Block2
       ( collectEvery
       , stringSum
       , mergeSort
       , removeAt
       ) where
       
       
removeAt :: Int -> [a] -> (Maybe a, [a])
removeAt x y = if x >= length y
               then (Nothing, y)
               else (Just (head (drop x y)), (take x y) ++ (drop (x + 1) y))
               
collectEvery :: Int -> [a] -> [[a]]
collectEvery x y = let predic f (_, z) = f ((mod z x) /= 0) 
                   in [fst (unzip (filter (predic ((&&) True)) (zip y [1..]))), fst (unzip (filter (predic not) (zip y [1..])))]

stringSum :: String -> Int
stringSum s = sum (map read $ words s)
                   
merge :: Ord a => ([a], [a]) -> [a]
merge ([], y) = y
merge (x, []) = x
merge (x:xs, y:ys) = if x < y
               then [x] ++ merge(xs, y:ys)
               else [y] ++ merge(x:xs, ys)

mergeSort :: Ord a => [a] -> [a]
mergeSort [] = []
mergeSort [x] = [x]
mergeSort x = let y = splitAt (length x `div` 2) x 
              in merge (mergeSort (fst y), mergeSort(snd y))