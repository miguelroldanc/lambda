import Data.Monoid
import Control.Monad.Writer
import Control.Monad.State
import Control.Monad
import Control.Applicative
import Data.Ratio
import Data.List (all)

-- Return a tuple from a function
isBigGang :: Int -> (Bool, String)
isBigGang x = (x > 9, "Compared gang size to 9")

applyLog :: (Monoid m) => (a,m) -> (a -> (b,m)) -> (b,m)
applyLog (x,log) f = let (y,newLog) = f x in (y,log `mappend` newLog)

-- Apply monads several times
type Food = String
type Price = Sum Int

addDrink :: Food -> (Food, Price)
addDrink "beans" = ("milk", Sum 25)
addDrink "jerky" = ("whiskey", Sum 99)
addDrink _ = ("beer", Sum 30)

-- Keep track of operations using writer monad
-- It is more efficient inserting the logs in reverse
gcdPlus :: Int -> Int -> Writer [String] Int
gcdPlus a b
    | b == 0 = do
        tell ["Finished with " ++ show a]
        return a
    | otherwise = do
        tell [show a ++ " mod " ++ show b ++ " = " ++ show (a `mod` b)]
        gcdPlus b (a `mod` b)


-- Efficient lists: DIFFERENCE LISTS
newtype DiffList a = DiffList { getDiffList :: [a] -> [a] }

toDiffList :: [a] -> DiffList a
toDiffList xs = DiffList (xs++)

fromDiffList :: DiffList a -> [a]
fromDiffList (DiffList f) = f []

-- The mappend operation has been moved from monoid
-- to semigroup. mappend is a synonym for <>
instance Semigroup (DiffList a) where
    (DiffList f) <> (DiffList g) = DiffList (f . g)
instance Monoid (DiffList a) where
    mempty = DiffList ([] ++)

-- Using State monad
type Stack = [Int]
pop :: State Stack Int
pop = state $ \(x:xs) -> (x,xs)

push :: Int -> State Stack ()
push a = state $ \xs -> ((),a:xs)

stackManip :: State Stack Int
stackManip = do
    push 3
    a <- pop
    pop

stackStuff :: State Stack ()
stackStuff = do
    a <- pop
    if a == 5
        then push 5
        else do
            push 3
            push 6

moreStack :: State Stack ()
moreStack = do
    a <- stackManip
    Control.Monad.when (a == 100) stackStuff

-- Filtering using monads
keepSmall :: Int -> Writer [String] Bool
keepSmall x
    | x < 4 = do
        tell ["Keeping " ++ show x]
        return True
    | otherwise = do
        tell [show x ++ " thrown away"]
        return False

-- Taking the powerset of a list
powerset :: [a] -> [[a]]
powerset xs = filterM (\x -> [True, False]) xs

-- Working with probabilities
newtype Prob a = Prob { getProb :: [(a,Rational)] } deriving Show
instance Functor Prob where
    fmap f (Prob xs) = Prob $ map (\(x,p) -> (f x,p)) xs

-- Define the monad for the prob newtype
flatten :: Prob (Prob a) -> Prob a
flatten (Prob xs) = Prob $ concat $ map multAll xs
    where multAll (Prob innerxs,p) = map (\(x,r) -> (x,p*r)) innerxs

-- Applicative has become the superclass for Monad. The instance
-- of the Applicative is needed  
instance Applicative Prob where
    pure x = Prob [(x,1%1)]
    (<*>) = ap
    
instance Monad Prob where
    return = pure
    m >>= f = flatten (fmap f m)

data Coin = Heads | Tails deriving (Show, Eq)

coin :: Prob Coin
coin = Prob [(Heads,1%2),(Tails,1%2)]

loadedCoin :: Prob Coin
loadedCoin = Prob [(Heads,1%10),(Tails,9%10)]

flipThree :: Prob Bool
flipThree = do
    a <- coin
    b <- coin 
    c <- loadedCoin
    return (all (==Tails) [a,b,c])