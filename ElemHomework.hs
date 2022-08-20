

elem' _ [] = False
elem' val (x:xs)
    | x == val      = True
    | otherwise      = elem' val xs 


elemf p lis = foldl (||) False (map (== p) lis)


par p lis = (l, r)
    where l = foldr (\x -> if p x then (x:) else id) [] lis
          r = foldr (\x -> if p x then id else (x:)) [] lis


foldL p res l = foldr (\b g x -> g (p x b)) id l res


all' pred l = foldl (\acc y -> if pred y then True && acc else False && acc) True l