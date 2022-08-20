
-- fibo 0 = 0
-- fibo 1 = 1 
-- fibo n = fibo (n-1) + fibo (n-2)

fibo :: Integral p => p -> p
fibo n 
    | n == 0 || n == 1  = n
    | n > 1  = fibo (n-1) + fibo (n-2) 
    | otherwise = error "Can't handle this case"


fib_aux i res res'
  | i == 10    = res
  | otherwise  = fib_aux (i+1) res' (res+res')

