{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE ViewPatterns  #-}

module Block3
       ( Day (..)
       , Creature (..)
       , CreatureType (..)
       , Vector (..)
       , Tree (..)
       , Nat (..)
       , verticalPrint
       , toList
       , nextDay
       , afterDays
       , isWeekend
       , daysToParty
       , partyKnight
       , businessDragon
       , result
       , fight
       , inc3
       , damageCreature
       , execFight
       , packVector
       , unpackVector
       , dist
       , lengthV
       , sumV
       , scal
       , vect
       , nToInteger
       , sizeTree
       , isEmpty
       , search
       , put
       , fromList
       ) where
       
import           Data.Char (isSpace)
import           Data.List (maximum)
import           Data.Semigroup
import           Data.Monoid (mempty, mappend)
       
       
data Day = Monday | Tuesday | Wednesday | Thursday | Friday | Saturday | Sunday
    deriving (Ord, Show, Eq, Enum, Bounded)

nextDay :: Day -> Day
nextDay d = head (tail ([d..maxBound] ++ [Monday]))

afterDays :: Int -> Day -> Day
afterDays 0 d = d
afterDays n d = afterDays (n - 1) (nextDay d)

isWeekend :: Day -> Bool
isWeekend d = (d == Saturday) || (d == Sunday)

daysToParty :: Day -> Int
daysToParty Friday = 0
daysToParty d = 1 + daysToParty(nextDay d)

data CreatureType = Knight | Monster 
    deriving (Show)
    
data Creature = Creature
    { whoIs :: CreatureType
    , damage :: Int
    , name :: String
    , health :: Day -> Int
    , power :: Day -> Int
    , speed :: Day -> Int
    }
    
partyKnight :: Creature
partyKnight = Creature  { whoIs = Knight
                        , name = "Knight of the Party"
                        , damage = 0
                        , health = \_ -> 1000
                        , power = \d -> (200 - 10 * daysToParty d)
                        , speed = \d -> (120 - 20 * daysToParty d)
                        }

businessDragon :: Creature
businessDragon = Creature   { whoIs = Monster
                            , name = "Sol!d Dragon"
                            , damage = 0
                            , health = \_ -> 800
                            , power = \d -> (if isWeekend d then 0 else 180)
                            , speed = \d -> (if isWeekend d then 0 else 100)
                            }
  
result :: (Creature, Creature, Int) -> String
result (c,k,d) = "Winner is " ++ name c ++ " in battle with " ++ name k ++ " in " ++ show d ++ " days!"
  
fight :: Creature -> Creature -> Day -> String
fight x y d = if (speed x) d > (speed y) d
              then result (execFight(x, y, d))
              else result (execFight(y, x, d))             
              
inc3 :: (a, a, Int) -> (a, a, Int)
inc3 (x, y, i) = (x, y, i + 1)

damageCreature :: Creature -> Int -> Creature
damageCreature c i = c { damage = (damage c) + i }
              
execFight :: (Creature, Creature, Day) -> (Creature, Creature, Int)
execFight (x, y, d) = if ((power x) d) > ((health y) d) - damage y
                      then (x, y, 0)
                      else inc3(execFight(damageCreature y (power x d), x, nextDay d))
                      
                      
data Vector a = Vector2D a a | Vector3D a a a
    deriving (Show)

packVector :: Vector a -> [a]
packVector (Vector2D x y)   = [x, y]
packVector (Vector3D x y z) = [x, y, z]

unpackVector :: [a] -> Vector a
unpackVector [] = (error "You need more args")
unpackVector [_] = unpackVector []
unpackVector [x, y]    = (Vector2D x y)
unpackVector [x, y, z] = (Vector3D x y z)
unpackVector (x:y:z:_) = (Vector3D x y z)

lengthV :: Floating a => Vector a -> a
lengthV v = sqrt (sum (map (**2) (packVector v)))

sumV :: Num a => Vector a -> Vector a -> Vector a
sumV x y = unpackVector (zipWith (+) (packVector x) (packVector y))

scal :: Num a => Vector a -> Vector a -> a
scal x y = sum (zipWith (*) (packVector x) (packVector y))

dist :: Floating a => Vector a -> Vector a -> a
dist x y = sqrt(sum (map (**2) (zipWith (-) (packVector x) (packVector y))))

vect :: Num a => Vector a -> Vector a -> Vector a
vect (Vector3D x y z) (Vector3D i j k)  = Vector3D (y * k - z * j) (z * i - x * k) (x * j - y * i)
vect (Vector2D x y) v                   = vect (Vector3D x y 0) v
vect v (Vector2D x y)                   = vect v (Vector3D x y 0)


data Nat = Z | S Nat
    deriving (Show)
    
instance Eq Nat
  where
    (S n) == (S k)  = n == k
    Z == Z          = True
    _ == _          = False
    
instance Ord Nat
  where
    (S n) > (S k)   = n > k
    _ > Z           = True
    Z > _           = False
    
    n >= k = n > k || n == k
    
    n <= k = not(n > k)
    
    n < k = (not (n > k)) &&  (not (n == k))
    
    compare (S n) (S k) = compare n k
    compare (S _) Z     = GT
    compare Z (S _)     = LT
    compare Z Z         = EQ

instance Num Nat
  where
    n + (S k)       = (S n) + k
    n + Z           = n
        
    n * (S k)       = n * k + n
    _ * Z           = Z
    
    (S n) - (S k)   = n - k
    n - Z           = n
    Z - _           = Z
    
    abs n           = n
    
    signum (S _)    = S Z
    signum Z        = Z
    
    fromInteger n   = if n > 0 
                      then (S Z) + fromInteger (n - 1)
                      else Z
    
nToInteger :: Nat -> Integer
nToInteger (S n) = 1 + nToInteger n
nToInteger Z     = 0
    

data Tree a = Leaf | Node a (Tree a) (Tree a)
    deriving (Show, Functor)

isEmpty :: Tree a -> Bool
isEmpty Leaf = True
isEmpty _    = False

sizeTree :: Tree a -> Int
sizeTree Leaf = 1
sizeTree (Node _ n k) = 1 + sizeTree n + sizeTree k

search :: Ord a => a -> Tree a -> Bool
search _ Leaf = False
search n (Node x y z)
    | n == x = True
    | n < x  = search n y
    | n > x  = search n z
    
put :: Ord a => a -> Tree a -> Tree a
put n Leaf = Node n Leaf Leaf
put n k@(Node x y z)
    | n == x = k
    | n < x  = Node x (put n y) z
    | n > x  = Node x y (put n z)
    
fromList :: Ord a => [a] -> Tree a
fromList [] = Leaf
fromList x  = put (head x) (fromList (tail x))




instance Foldable Tree 
  where
   foldMap _ Leaf = mempty
   foldMap f (Node k l r) = foldMap f l `mappend` f k `mappend` foldMap f r
   
   foldr _ z Leaf = z
   foldr f z (Node k l r) = foldr f (f k (foldr f z r)) l

toList :: Tree a -> [a]   
toList = foldr (\x y -> [x] ++ y) []

instance Ord a => Semigroup (Tree a)
  where
    (<>) a b = fromList (toList a ++ toList b)

    
instance Ord a => Monoid (Tree a)
  where
    mempty = Leaf
    mappend a b = fromList (toList a ++ toList b)






verticalPrint :: Show a => Tree a -> String
verticalPrint = unlines . rowPrinter . fmap show

type TreeRows = [String]

rowPrinter :: Tree String -> TreeRows
rowPrinter Leaf                  = []
rowPrinter (Node key Leaf  Leaf) = [key]
rowPrinter (Node key left  Leaf) = connectOneChild key left
rowPrinter (Node key Leaf right) = connectOneChild key right
rowPrinter (Node key left right) =
    let lr@(ltop:_)  = rowPrinter left
        rr@(rtop:_)  = rowPrinter right

        ledgePos     = labelMidPosition ltop
        redgePos     = labelMidPosition rtop

        leftWidth    = 1 + maximum (map length lr)
        connectorLen = leftWidth + redgePos - 1 - ledgePos
        connector    = nspaces (ledgePos + 1) ++ replicate connectorLen '-'

        leftSubTree  = upEdge ledgePos : lr
        rightSubTree = upEdge redgePos : rr
        childrenRows = mergeChildren leftWidth leftSubTree rightSubTree
    in attachRows key (connector:childrenRows)

connectOneChild :: String -> Tree String -> TreeRows
connectOneChild label (rowPrinter -> rows) = attachRows label rows

attachRows :: String -> TreeRows -> TreeRows
attachRows label subTree@(top:_) =
    let labelMid    = labelMidPosition label
        topLabelMid = labelMidPosition top
        shortEdge   = upEdge topLabelMid
        subTreeRows = shortEdge : subTree
        padding     = abs (topLabelMid - labelMid)
        (cur, tree) = if topLabelMid < labelMid
                      then (label, map (moveRight padding) subTreeRows)
                      else (moveRight padding label, subTreeRows)
    in cur : tree
attachRows _ _ = error "Algorithm error: attach call on empty subtree"

middle :: Int -> Int
middle x = x `div` 2

labelMidPosition :: String -> Int
labelMidPosition label =
    let (spaces, value) = span isSpace label
        valueMid        = middle $ length value
    in length spaces + valueMid

nspaces :: Int -> String
nspaces n = replicate n ' '

upEdge :: Int -> String
upEdge padding = nspaces padding ++ "|"

moveRight :: Int -> String -> String
moveRight n = (nspaces n ++)

fillRight :: Int -> String -> String
fillRight len s = s ++ nspaces (len - length s)

mergeChildren :: Int -> TreeRows -> TreeRows -> TreeRows
mergeChildren lWidth = scanDown
  where
    scanDown :: TreeRows -> TreeRows -> TreeRows
    scanDown    []     []  = []
    scanDown     l     []  = l
    scanDown    []  (r:rs) = (nspaces   lWidth   ++ r) : scanDown [] rs
    scanDown (l:ls) (r:rs) = (fillRight lWidth l ++ r) : scanDown ls rs
        
