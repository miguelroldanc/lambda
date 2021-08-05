data Tree a = Empty | Node a (Tree a) (Tree a) deriving (Show)

freeTree :: Tree Char
freeTree = 
    Node 'M'
        (Node 'O'
            (Node 'L' 
                (Node 'N' Empty Empty)
                (Node 'P' Empty Empty)
            )
            (Node 'Y'
                (Node 'S' Empty Empty)
                (Node 'A' Empty Empty)
            )
        )
        (Node 'L'
            (Node 'W'
                (Node 'C' Empty Empty)
                (Node 'R' Empty Empty)
            )
            (Node 'A'
                (Node 'A' Empty Empty)
                (Node 'C' Empty Empty)
            )
        )

data Direction = L | R deriving (Show)
type Directions = [Direction]

-- Change the top of the tree with a 'P'
changeTop :: Directions -> Tree Char -> Tree Char
changeTop (L:ds) (Node x l r) = Node x (changeTop ds l) r
changeTop (R:ds) (Node x l r) = Node x l (changeTop ds r)
changeTop [] (Node _ l r) = Node 'P' l r

-- Access index 
elemAt :: Directions -> Tree a -> a
elemAt (L:ds) (Node _ l _) = elemAt ds l
elemAt (R:ds) (Node _ _ r) = elemAt ds r
elemAt [] (Node x _ _) = x

-- Use hints for discovering the tree from the root
-- type Breadcrumbs = [Direction]
-- goLeft :: (Tree a, Breadcrumbs) -> (Tree a, Breadcrumbs)
-- goLeft (Node _ l _, bs) = (l, L:bs)
-- 
-- goRight :: (Tree a, Breadcrumbs) -> (Tree a, Breadcrumbs)
-- goRight (Node _ _ r, bs) = (r, R:bs)

-- Use hints for moving up the tree
data Crumb a = LeftCrumb a (Tree a) | RightCrumb a (Tree a) deriving (Show)
type Breadcrumbs a = [Crumb a]

-- goLeft :: (Tree a, Breadcrumbs a) -> (Tree a, Breadcrumbs a)
-- goLeft (Node x l r, bs) = (l, LeftCrumb x r:bs)
-- 
-- goRight :: (Tree a, Breadcrumbs a) -> (Tree a, Breadcrumbs a)
-- goRight (Node x l r, bs) = (r, RightCrumb x l:bs)
-- 
-- goUp :: (Tree a, Breadcrumbs a) -> (Tree a, Breadcrumbs a)
-- goUp (t, LeftCrumb x r:bs) = (Node x t r, bs)
-- goUp (t, RightCrumb x l:bs) = (Node x l t, bs)

-- Construct pair with the data structure and
-- its surroundings
type Zipper a = (Tree a, Breadcrumbs a)

-- Manipulating trees under focus
modify :: (a -> a) -> Zipper a -> Zipper a
modify f (Node x l r, bs) = (Node (f x) l r, bs)
modify f (Empty, bs) = (Empty, bs)

-- Walking through the tree using zippers
goLeft :: Zipper a -> Maybe (Zipper a)
goLeft (Node x l r, bs) = Just (l, LeftCrumb x r:bs)
goLeft (Empty, _) = Nothing 

goRight :: Zipper a -> Maybe (Zipper a)
goRight (Node x l r, bs) = Just (r, RightCrumb x l:bs)
goRight (Empty, _) = Nothing 

goUp :: Zipper a -> Maybe (Zipper a)
goUp (t, LeftCrumb x r:bs) = Just (Node x t r, bs)
goUp (t, RightCrumb x l:bs) = Just (Node x l t, bs)