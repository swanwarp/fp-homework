module Block4
       ( splitOn
       , joinWith
       ) where


splitOn :: Eq a => a -> [a] -> [[a]]
splitOn n x = let f y z = if y == n
                          then [[]] ++ z
                          else [([y] ++ head z)] ++ (tail z)
              in foldr f [[]] x

joinWith :: Eq a => a -> [[a]] -> [a]
joinWith n x = let f y z = if z /= [n]
                           then if y /= []
                                then y ++ [n] ++ z
                                else z
                           else y
               in foldr f [n] x