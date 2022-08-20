data Tree a = Empty | Node (Tree a) a (Tree a)
class IfValue a where boolVal :: a -> Bool

instance IfValue Int where 
    boolVal 0 = False
    boolVal _ = True 

instance IfValue Integer where
    boolVal 0 = False
    boolVal _ = True

instance IfValue Double where
    boolVal 0 = False
    boolVal _ = True

instance IfValue (Maybe a) where
    boolVal Nothing = False
    boolVal _      = True

instance IfValue [a] where
    boolVal [] = False
    boolVal _  = True 



instance IfValue Bool where
    boolVal = id

instance IfValue Char where
    boolVal '\NUL' = False 
    boolVal _      = True

instance IfValue (Tree a) where 
    boolVal Empty = False
    boolVal _    = True

instance (IfValue a , IfValue b) => IfValue (a,b)
    where boolVal (a,b) = boolVal a || boolVal b

instance IfValue () where 
    boolVal _ = False



maybeMap :: (a -> b) -> Maybe a -> Maybe b
maybeMap _ Nothing = Nothing
maybeMap func (Just x) = Just (func x)



pairMapA :: (a -> a') -> (a, b) -> (a', b)
pairMapA func = swap . pairMapB func . swap

pairMapB :: (b -> b') -> (a, b) -> (a, b')
pairMapB func (x, y) = (x, func y)

swap :: (a, b) -> (b, a)
swap (x, y) = (y, x)


