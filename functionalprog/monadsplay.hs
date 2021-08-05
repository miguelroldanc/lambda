-- Understanding the use of >>=
applyMaybe :: Maybe a -> (a -> Maybe b) -> Maybe b
applyMaybe Nothing f = Nothing
applyMaybe (Just x) f = f x

-- The tightrope man and the birds
type Birds = Int
type Pole = (Birds,Birds)
-- Birds on each side of the pole
landLeft :: Birds -> Pole -> Maybe Pole
landLeft n (left,right)
    | abs ((left + n) - right) < 4 = Just (left + n,right)
    | otherwise = Nothing 

landRight :: Birds -> Pole -> Maybe Pole
landRight n (left,right)
    | abs ((right + n) - left) < 4 = Just (left,right + n)
    | otherwise = Nothing 

-- Illustration of do action replacing >>= operator and lambda expressions
foo :: Maybe String 
foo = do
    x <- Just 3
    y <- Just "!"
    Just (show x ++ y)

-- Routine of the tightwalker
routine :: Maybe Pole
routine = do
    start <- return (0,0)
    first <- landLeft 2 start
    second <- landRight 2 first
    landLeft 1 second

-- More examples on do notation
justWrite :: Maybe Char
justWrite = do
    (x:xs) <- Just "hey there"
    return x

-- Making tuples
listOfTuples :: [(Int, Char)]
listOfTuples = do
    n <- [1,2]
    ch <- ['a','b']
    return (n,ch)

