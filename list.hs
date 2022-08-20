-- list.hs <-- this line is a comment !!

--len [] = 0
--len (x : xs) = x : len xs

len ([], lis) = lis
len ((x:xs) , lis) = x : (len (xs , lis))