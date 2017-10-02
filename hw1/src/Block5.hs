module Block5
       ( maybeConcat
       , monoidConcat
       , eitherConcat
       ) where

import           Data.Maybe (fromMaybe)
import           Data.Monoid (mempty, mappend)
import           Data.Either (lefts, rights)
import           Data.Semigroup


maybeConcat :: [Maybe [a]] -> [a]
maybeConcat [] = []
maybeConcat x = (fromMaybe [] (head x)) ++ maybeConcat (tail x)

monoidConcat :: Monoid a => [a] -> a
monoidConcat [] = mempty
monoidConcat x = mappend (head x) (monoidConcat (tail x))

eitherConcat :: (Monoid a, Monoid b) => [Either a b] -> (a, b)
eitherConcat x = (monoidConcat (lefts x), monoidConcat (rights x))

data NonEmpty a = a :| [a]

instance Semigroup (NonEmpty a)
  where
    (x :| xs) <> ~(y :| ys) = x :| (xs ++ y : ys)
    
newtype Identity a = Identity {runIdentity :: a}

instance Semigroup a => Semigroup (Identity a)
  where
    (<>) (Identity x) (Identity y) = Identity (x <> y)

instance Monoid a => Monoid (Identity a)
  where
    mempty = Identity mempty
    mappend (Identity x) (Identity y) = Identity (mappend x y) 