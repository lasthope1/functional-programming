
len_comp l = sum [1 | x <- l]



pair_even [] _ = []
pair_even (x:xs) lis = pair_aux x lis++pair_even xs lis 
    where
    pair_aux _ [] = []
    pair_aux x (y:ys) 
        | even $ x + y   = (x,y) : pair_aux x ys
        | otherwise      = pair_aux x ys




