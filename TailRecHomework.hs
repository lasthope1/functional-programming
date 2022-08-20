

-- rev l = rev_aux l []
--     where 

list_map _ [] = []
list_map opr (x:xs) = (opr x) : list_map opr xs 



-- Tail Recursion
list_map' opr lis = list_map_aux opr lis []
    where 
        list_map_aux _ []          res = res
        list_map_aux opr (x:xs)     res = list_map_aux opr xs (res ++ [(opr x)])


zipper l1 l2 = zipper_aux l1 l2 []
    where
        zipper_aux [] _     res = res
        zipper_aux _ []     res = res
        zipper_aux (x:xs) (y:ys)    res = zipper_aux xs ys (res ++ [(x,y)])


