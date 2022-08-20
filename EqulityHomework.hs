
data Tree a = Empty | Node (Tree a) a (Tree a) deriving(Show)


treeEquals :: Eq a => Tree a -> Tree a -> Bool

treeEquals Empty Empty = True
treeEquals (Node l1 v1 r1) (Node l2 v2 r2) = 
    v1 == v2 && 
    treeEquals l1 l2 && 
    treeEquals r1 r2
treeEquals _ _ = False


listEquals :: Eq a => [a] -> [a] -> Bool

listEquals [] [] = True
listEquals (x : xs) (y : ys) =
    x == y && listEquals xs ys
listEquals _ _ = False


pairEquals :: (Eq a, Eq b) => (a , b) -> (a , b) -> Bool

pairEquals (x1 , y1) (x2 , y2) = x1 == x2 && y1 == y2


maybeEquals :: Eq a => Maybe a -> Maybe a -> Bool 

maybeEquals Nothing Nothing = True
maybeEquals (Just v1) (Just v2) = v1 == v2 
maybeEquals _ _ = False



