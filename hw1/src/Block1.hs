{-# LANGUAGE ScopedTypeVariables #-}

module Block1
       ( order3
       , highestBit
       , smartReplicate
       , contains
       ) where
       
order3 :: Ord a => (a, a, a) -> (a, a, a)
order3 (a, b, c) = (min (min a b) c, max (max (min a b) (min a c)) (min b c), max (max a b) c)

highestBit :: Int -> (Int, Int)
highestBit a = let exp2 x = if 2^(x + 1) <= a 
                            then exp2 (x + 1) 
                            else (2^x, x) 
               in exp2 0

smartReplicate :: [Int] -> [Int]
smartReplicate a = foldMap (\x -> replicate x x) a

contains :: Eq a => a -> [[a]] -> [[a]]
contains x y = filter (elem x) y



