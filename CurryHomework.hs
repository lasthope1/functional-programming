

zipper' [] _ = []
zipper' _ [] = []
zipper' (x:xs) (y:ys) = (x , y) : zipper' xs ys


fac 0 = 1 
fac n = n * fac (n-1)