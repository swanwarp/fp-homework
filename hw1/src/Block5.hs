module Block5
       ( maybeConcat
       , monoidConcat
       , eitherConcat
       ) where

import           Data.Maybe (fromMaybe)
import           Data.Monoid (mempty, mappend)
import           Data.Either (lefts, rights)

maybeConcat :: [Maybe [a]] -> [a]
maybeConcat [] = []
maybeConcat x = (fromMaybe [] (head x)) ++ maybeConcat (tail x)

monoidConcat :: Monoid a => [a] -> a
monoidConcat [] = mempty
monoidConcat x = mappend (head x) (monoidConcat (tail x))

eitherConcat :: (Monoid a, Monoid b) => [Either a b] -> (a, b)
eitherConcat x = (monoidConcat (lefts x), monoidConcat (rights x))