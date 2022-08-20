

join ([], ls) = ls
join ((x:xs) , ls) = x : join (xs , ls)


zipper ([] , _) = []
zipper (_ , []) = []
zipper (x:xs , y:ys) = ( x , y ) : zipper (xs , ys)
-- type of  zipper :: ([a] , [b]) -> [(a,b)]


rev [] = []
rev (x:xs) = join( rev(xs) , [x])  -- Idea Number 1 
-- rev (lis) = last lis : rev (init lis)  -- Idea Number 2
-- type of rev :: [a] -> [a]
