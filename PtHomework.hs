

partition p [] = ([], [])
partition p (x:xs)
    | p x       = (x:l, r)
    | otherwise  = (l, x:r)
    where (l, r) = partition p xs 





filter' pred = fst . partition pred



qSort [] = []
qSort (x:xs) = qSort l ++ [x] ++ qSort r
    where (l , r) = partition (<x) xs



