import qualified Data.Map as Map
-- Initial chapters

-- Functions
-- Function with one parameter
doubleMe x = x + x
-- Function with several parameters
doubleUs x y = doubleMe x + doubleMe y
-- Function with if-else statement
doubleSmallNumber x = (if x > 100 then x else x*2) + 1
-- Function with no parameters
conanO'Brien = "It's me Conan O'Brien!"

-- Lists
-- Creating lists (in ghci we can use the keyword let)
lostNumbers = [4,8,15,16,23,42]
lista = [[1,2,3,4],[5,3,3,3],[9,8,7]]
noun = ["hobo","frog","pope"]
adjectives = ["lazy","grouchy","scheming"]
boomBang xs = [if x < 10 then "BOOM!" else "BANG!" | x <- xs, odd x]
length' xs = sum [1 | _ <- xs]
removeNonUppercase st = [c | c <- st, c `elem` ['A'..'Z']]
rightTriangles = [ (a,b,c) | c <- [1..10], b <- [1..c], a <- [1..b], a^2 + b^2 == c^2, a+b+c == 24]


-- Chapter 4: Syntax in functions

factorial :: (Integral a) => a -> a -- Another quick solution is product [1..n]
factorial 0 = 1
factorial n = n * factorial (n-1)


lucky :: (Integral a) => a -> String
lucky 7 = "LUCKY NUMBER SEVEN!"
lucky x = "Sorry, you're out of luck, pal!"

sayMe :: (Integral a) => a -> String
sayMe 1 = "One!"
sayMe 2 = "Two!"
sayMe x = "Either 1 or 2"

charName :: Char -> String
charName 'a' = "Albert"
charName 'b' = "Broseph"

addVectors :: (Num a) => (a,a) -> (a,a) -> (a,a)
addVectors (x1, y1) (x2,y2) = (x1 + x2, y1 + y2)

first :: (a, b, c) -> a
first (x, _, _) = x

lengthNew :: (Num b) => [a] -> b
lengthNew [] = 0
lengthNew (_:xs) = 1 + lengthNew xs

sum' :: (Num a) => [a] -> a
sum' [] = 0
sum' (x:xs) = x + sum' xs

capital :: String -> String 
capital "" = "Empty string"
capital all@(x:xs) = "First letter of " ++ all ++ " is " ++ [x]

bmiTell :: (RealFloat a) => a -> String
bmiTell bmi
    | bmi <= 18.5 = "You're underweight, you emo, you!"
    | bmi <= 25.0 = "You're supposedly normal. Pff, I bet you're ugly!"
    | bmi <= 30.0 = "You're fat! Lose some weight, fatty!"
    | otherwise   = "You're whale, congratulations!"

bmiTell' :: (RealFloat a) => a -> a -> String
bmiTell' weight height
    | weight / height ^ 2 <= 18.5 = "You're underweight, you emo, you!"
    | weight / height ^ 2 <= 25.0 = "You're supposedly normal. Pff, I bet you're ugly!"
    | weight / height ^ 2 <= 30.0 = "You're fat! Lose some weight, fatty!"
    | otherwise   = "You're whale, congratulations!"

max' :: (Ord a) => a -> a -> a
max' a b  | a > b = a | otherwise = b

myCompare :: (Ord a) => a -> a -> Ordering
a `myCompare` b
    | a > b = GT
    | a == b = EQ 
    | otherwise = LT

bmiImproved :: (RealFloat a) => a -> a -> String 
bmiImproved weight height
    | bmi <= skinny = "You're okay"
    | bmi <= normal = "You're fine"
    | bmi <= fat = "You're fantastic"
    | otherwise = "Everything is okay"
    where bmi = weight / height ^ 2
          (skinny, normal, fat) = (18.5, 25.0, 30.0)

initials :: String -> String -> String 
initials firstname lastname = [f] ++ ". " ++ [l] ++ "."
    where (f:_) = firstname
          (l:_) = lastname

calcBmis :: (RealFloat a) => [(a, a)] -> [a]
calcBmis xs = [bmi w h | (w, h) <- xs]
    where bmi weight height = weight / height ^ 2

cylinder :: (RealFloat a) => a -> a -> a
cylinder r h = 
    let sideArea = 2 * pi * r * h
        topArea = pi * r^2
    in  sideArea + 2 * topArea

calcBmis' :: (RealFloat a) => [(a, a)] -> [a]
calcBmis' xs = [bmi | (w, h) <- xs, let bmi = w / h ^ 2, bmi >= 25.0]

describeList :: [a] -> String 
describeList xs = "The list is " ++ case xs of [] -> "empty"
                                               [x] -> "singleton"
                                               xs -> "long list"

describeList' :: [a] -> String 
describeList' xs = "The list is " ++ what xs
    where what [] = "empty"
          what [x] = "singleton"
          what xs = "long list"


-- Chapter 5: Recursion

maximum' :: (Ord a) => [a] -> a
maximum' [] = error "maximum of empty list"
maximum' [x] = x
maximum' (x:xs)
    | x > maxTail = x
    | otherwise = maxTail
    where maxTail = maximum' xs

repeat' :: a -> [a]
repeat' x = x:repeat' x

-- Same functionality as replicate
-- It shows another way to get the same result
repeatNTimes :: (Num a) => Int -> a -> [a]
repeatNTimes x y 
    | x < 1 = []
    | otherwise = take x (repeat y)

quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort (x:xs) = 
    let smallerSorted = quicksort [a | a <- xs, a <= x]
        biggerSorted = quicksort [a | a <- xs, a > x]
    in  smallerSorted ++ [x] ++ biggerSorted


-- Chapter 6: high order functions

multThree :: (Num a) => a -> a -> a -> a
multThree x y z = x * y * z

compareWithHundred :: (Num a, Ord a) => a -> Ordering 
compareWithHundred = compare 100

divideByTen :: (Floating a) => a -> a
divideByTen = (/10)

isUpperAlphanum :: Char -> Bool
isUpperAlphanum = (`elem` ['A'..'Z'])

applyTwice :: (a -> a) -> a -> a
applyTwice f x = f (f x)

zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' _ [] _ = []
zipWith' _ _ [] = []
zipWith' f (x:xs) (y:ys) = f x y : zipWith' f xs ys

flip' :: (a -> b -> c) -> (b -> a -> c)
flip' f y x = f x y

-- New version of quicksort
quicksort2 :: (Ord a) => [a] -> [a]
quicksort2 [] = []
quicksort2 (x:xs) =
    let smallerSorted = quicksort (filter (<=x) xs)
        biggerSorted = quicksort (filter (>x) xs)
    in smallerSorted ++ [x] ++ biggerSorted

chain :: (Integral a) => a -> [a]
chain 1 = [1]
chain n
    | even n = n:chain (n `div` 2)
    | odd n = n:chain (n*3 + 1)

numLongChains :: Int
numLongChains = length (filter isLong (map chain [1..100]))
    where isLong xs = length xs > 15

numLongChains' :: Int
numLongChains' = length (filter (\xs -> length xs > 15) (map chain [1..100]))

addThreeLambda :: (Num a) => a -> a -> a -> a
addThreeLambda = \x -> \y -> \z -> x + y + z

elemFoldl :: (Eq a) => a -> [a] -> Bool
elemFoldl y ys = foldl (\acc x -> if x == y then True else acc) False ys

map' :: (a -> b) -> [a] -> [b]
map' f xs = foldr (\x acc -> f x : acc) [] xs

oddSquareSum :: Integer
oddSquareSum = sum . takeWhile (<30) . filter odd . map (^2) $ [1..]


-- Chapter 8: Making Our Own Types and Typeclasses

-- initial Shape
-- data Shape = Circle Float Float Float | Rectangle Float Float Float Float deriving (Show)
-- initial surface function
-- surface :: Shape -> Float
-- surface (Circle _ _ r) = pi * r ^ 2
-- surface (Rectangle x1 y1 x2 y2) = (abs $ x2 - x1) * (abs $ y2 - y1)

-- New structures for creating a circle and a rectangle

data Point = Point Float Float deriving (Show) -- Point is a type, Shape is a type and Show is a typeclass.
data Shape = Circle Point Float | Rectangle Point Point deriving (Show) -- Circle and Rectangle are value constructors

-- Function for the area of a Shape object
surface :: Shape -> Float
surface (Circle _ r) = pi * r ^ 2
surface (Rectangle (Point x1 y1) (Point x2 y2)) = (abs $ x2 - x1) * (abs $ y2 - y1)

-- Function for moving a Shape object
nudge :: Shape -> Float -> Float -> Shape
nudge (Circle (Point x y) r) a b = Circle (Point (x+a) (y+b)) r
nudge (Rectangle (Point x1 y1) (Point x2 y2)) a b = Rectangle (Point (x1+a) (y1+b)) (Point (x2+a) (y2+b))

-- Aux functions for base Shapes
baseCircle :: Float -> Shape
baseCircle r = Circle (Point 0 0) r

baseRect :: Float -> Float -> Shape
baseRect width height = Rectangle (Point 0 0) (Point width height)


-- Creating a Person
-- initial structure for a person
-- data Person = Person String String Int Float String String deriving (Show)
-- New version improved

data Person = Person { firstName :: String
                     , lastName :: String
                     , age :: Int
                     , height :: Float
                     , phoneNumber :: String
                     , flavor :: String
                     } deriving (Eq, Show, Read)

-- Getters for the attributes
-- firstName :: Person -> String
-- firstName (Person firstname _ _ _ _ _) = firstname

-- lastName :: Person -> String
-- lastName (Person _ lastname _ _ _ _) = lastname

-- age :: Person -> Int
-- age (Person _ _ age _ _ _) = age

-- height :: Person -> Float
-- height (Person _ _ _ height _ _) = height

-- phoneNumber :: Person -> String
-- phoneNumber (Person _ _ _ _ phone _) = phone

-- flavor :: Person -> String
-- flavor (Person _ _ _ _ _ flavor) = flavor

-- Record syntax is useful for complex structures
-- However for a data Vector = Vector Int Int Int it's not that useful

-- TYPE PARAMETERS
data Maybe a = Nothing | Just a deriving (Show)

-- Nothing is polymorphic like []
-- That's why we can do [1,2,3] ++ [] or ["ha","ha","ha"] ++ []

-- 3D vector
data Vector a = Vector a a a deriving (Show)

-- Several functions for the vector
vplus :: (Num t) => Vector t -> Vector t -> Vector t
(Vector i j k) `vplus` (Vector l m n) = Vector (i+l) (j+m) (k+n)

vectMult :: (Num t) => Vector t -> t -> Vector t
(Vector i j k) `vectMult` m = Vector (i*m) (j*m) (k*m)

scalarMult :: (Num t) => Vector t -> Vector t -> t
(Vector i j k) `scalarMult` (Vector l m n) = i*l + j*m + k*n


data Day = Monday | Tuesday | Wednesday | Thursday | Friday | Saturday | Sunday
           deriving (Eq, Ord, Show, Read, Bounded, Enum)

data Either a b = Left a | Right b deriving (Eq, Ord, Read, Show)
data LockerState = Taken | Free deriving (Show, Eq)
type Code = String
type LockerMap = Map.Map Int (LockerState, Code)

lockerLookUp :: Int -> LockerMap -> Prelude.Either String Code
lockerLookUp lockerNumber map = 
    case Map.lookup lockerNumber map of
        Prelude.Nothing -> Prelude.Left $ "Locker number " ++ show lockerNumber ++ " doesn't exist!"
        Prelude.Just (state, code) -> if state /= Taken
                              then Prelude.Right code
                              else Prelude.Left $ "Locker " ++ show lockerNumber ++ " is already taken!"


lockers :: LockerMap
lockers = Map.fromList
    [(100, (Taken, "A"))
    ,(101, (Free, "B"))
    ,(102,(Free,"CD"))
    ,(108,(Taken,"ABC"))]

data Tree a = EmptyTree | Node a (Tree a) (Tree a) deriving (Show, Read, Eq)

-- Tree with only one node
singleton :: a -> Tree a
singleton x = Node x EmptyTree EmptyTree

-- Tree construction
treeInsert :: (Ord a) => a -> Tree a -> Tree a
treeInsert x EmptyTree = singleton x
treeInsert x (Node a left right)
    | x == a = Node x left right
    | x < a  = Node a (treeInsert x left) right
    | x > a  = Node a left (treeInsert x right)

-- Check element in tree
treeElem :: (Ord a) => a -> Tree a -> Bool
treeElem x EmptyTree = False 
treeElem x (Node a right left)
    | x == a = True
    | x < a  = treeElem x left
    | x > a  = treeElem x right


-- Typeclasses 102
data TrafficLight = Red | Yellow | Green
instance Eq TrafficLight where
    Red == Red = True
    Green == Green = True
    Yellow == Yellow = True
    _ == _ = False

instance Show TrafficLight where
    show Red = "Red light"
    show Green = "Green light"
    show Yellow = "Yellow light"

-- Weak typeclasses (Weakly typed style like JavaScript)
class YesNo a where
    yesno :: a -> Bool

instance YesNo Integer where
    yesno 0 = False
    yesno _ = True 

instance YesNo [a] where
    yesno [] = False
    yesno _ = True

instance YesNo Bool where
    yesno = id

instance YesNo TrafficLight where
    yesno Red = False
    yesno _ = True

-- Functor: Map things over
class Functor f where 
    fmap :: (a -> b) -> f a -> f b

-- Define functor for lists
instance Main.Functor [] where
    fmap = map

-- Kinds
class Tofu t where 
    tofu :: j a -> t a j

data Frank a b = Frank {frankField :: b a} deriving (Show)

instance Tofu Frank where
    tofu x = Frank x