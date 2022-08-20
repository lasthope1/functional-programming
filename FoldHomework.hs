

revl x = foldl (\y ys -> ys:y) [] x





revr x = foldr (\y ys -> ys++[y]) [] x





map' p = foldr (\y ys -> (p y):ys) []





filter' p = foldr (\y ys -> if p y then y:ys else ys) []




