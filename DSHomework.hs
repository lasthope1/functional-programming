

data Tree a = Empty | Node (Tree a) a (Tree a) deriving(Show)

treeMap _ Empty = Empty
treeMap p (Node l v r) = Node (treeMap p l) (p v) (treeMap p r)


bt = Node (Node (Node Empty 2 Empty) 4 (Node Empty 3 Empty)) 5 (Node (Node Empty 7 Empty) 6 (Node Empty 8 Empty))


-- foldrIn p res Empty = res
-- foldrIn p res (Node Empty v Empty) = p v res
-- foldrIn p res (Node l v r) = foldrIn p (p v (foldrIn p res r)) l


--preorder
foldlPre p res Empty = res
foldlPre p res (Node Empty v Empty) = p res v
foldlPre p res (Node l v r) = foldlPre p (foldlPre p (p res v) l) r

foldrPre p res Empty = res 
foldrPre p res (Node Empty v Empty) = p v res
foldrPre p res (Node l v r) = p v (foldrPre p (foldrPre p res r) l)

--inorder
foldlIn p res Empty = res 
foldlIn p res (Node Empty v Empty) = p res v
foldlIn p res (Node l v r) = foldlIn p (p (foldlIn p res l) v) r

foldrIn p res Empty = res
foldrIn p res (Node Empty v Empty) = p v res
foldrIn p res (Node l v r) = foldrIn p (p v (foldrIn p res r)) l

--postorder
foldlPost p res Empty = res 
foldlPost p res (Node Empty v Empty) = p res v
foldlPost p res (Node l v r) = p (foldlPost p (foldlPost p res l) r) v

foldrPost p res Empty = res
foldrPost p res (Node Empty v Empty) = p v res
foldrPost p res (Node l v r) = foldrPost p (foldrPost p (p v res) r) l


height Empty = 0 
height (Node l _ r) = 1 + max (height l) (height r)

isBST Empty = False
isBST (Node l v r) = isBST l && isBST r && helpfn (<=v) l && helpfn (>=v) r
    where 
        helpfn _ Empty = True
        helpfn p (Node ls vs rs) = helpfn p ls && p vs && helpfn p rs



