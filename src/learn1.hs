take' :: (Num i, Ord i) => i -> [a] -> [a]  
take' n _  
    | n <= 0   = []  
take' _ []     = []  
take' n (x:xs) = x : take' (n-1) xs 

reverse' :: [a] -> [a]
reverse' [] = []
reverse' (x:xs) = reverse' xs ++ [x]

repeat' :: a -> [a]  
repeat' x = x:repeat' x

quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort (x:xs) = (quicksort [e | e <- xs, e < x]) ++ [x] ++ (quicksort [e | e <- xs,  e >= x])


chain :: (Integral a) => a -> [a]  
chain 1 = [1]  
chain n  
    | even n =  n:chain (n `div` 2)  
    | odd n  =  n:chain (n*3 + 1)


numLongChains :: Int
numLongChains = length (filter (>15) ( map length  (map chain [1..100] )))


zipWith' :: (Ord a) => (a->a->a) -> [a] -> [a] -> [a]
zipWith' _ _ [] = []
zipWith' _ [] _ = []
zipWith' f (x:xs) (y:ys) = [f x y] ++ (zipWith' f xs ys)


map' :: (Ord a) => (a->a) -> [a] -> [a]
map' f = foldr (\x acc -> f x : acc) []

filter' :: (Ord a) => (a->Bool) -> [a] -> [a]
filter' f  = foldr (\x acc -> if f x then x : acc else acc) []


intersperse' :: (Ord a) => a -> [a] -> [a]
intersperse' c (t:ts)  = t : (foldr (\x acc ->  [c,x] ++ acc  ) [] ts)

intercalate' :: (Ord a) => [a] -> [[a]] -> [a]
intercalate' b (t:ts) = t ++ (foldr (\x acc -> b ++ x ++ acc) [] ts) 

