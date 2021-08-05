-- The C stands for Counter
data CMaybe a = CNothing | CJust Int a deriving (Show)

instance Functor CMaybe where
    fmap f CNothing = CNothing
    fmap f (CJust counter x) = CJust (counter+1) (f x)

-- The difference between data and newtype is that newtype
-- has only one constructor and the values are represented
-- in the same way as the original values