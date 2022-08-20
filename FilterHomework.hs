
join ([], ls) = ls
join ((x:xs) , ls) = x : join (xs , ls)

filter_concat _ [] = []
filter_concat func (x:xs) 
    | func x       = x ++ filter_concat func xs 
    | otherwise     = filter_concat func xs


-- filter_concat' _ [] = []
filter_concat' func lis = concat $ filter func lis


take_while _ [] = []
take_while func (x:xs)
    | func x         = x : take_while func xs
    | otherwise      = []

