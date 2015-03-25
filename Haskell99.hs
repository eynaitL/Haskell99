myLast :: [a]->a
myLast [] = error "Empty List"
myLast (x:[]) = x
myLast (x:xs) = myLast xs

elementAt xs k = snd (foldl (\acc x->case acc of (0, y)->(0, y) ; (1, _) -> (0, x) ; (k, _) -> (k-1, x)) (k, head xs) xs)

elementAt' :: Integral b=>[a]->b->a
elementAt' [] _ = error "Empty List or Index out of bound"
elementAt' (x:_) 1 = x
elementAt' (_:xs) k = elementAt' xs (k-1)

myLength = sum.map (\x->1)

myReverse = foldl (\acc x->x:acc) []

myReverse'' xs = foldr (\x fId empty -> fId (x : empty)) id xs

isPalindrome xs = foldl (&&) True $ map (\(x,y) -> x == y) $ zip xs (myReverse xs)

data NestedList a = Elem a | List [NestedList a]
flatten :: NestedList a->[a]
flatten (Elem x) = [x]
flatten (List xs) = foldr (\x acc->(flatten x)++acc) [] xs

compress :: Eq a=>[a] -> [a]
compress [] = []
compress [x] = [x]
compress (x:y:xs)
         | x==y = compress (y:xs)
         | otherwise = x:compress (y:xs)

compress' :: Eq a=>[a] -> [a]
compress' = foldr f []
            where f x [] = [x]
            	  f x acc@(y:ys) 
                   | x == y = acc
                   | otherwise = x:acc

pack :: Eq a=>[a] -> [[a]]
pack xs = foldr f [[]] xs
          where f x [[]] = [[x]]
                f x acc@(y:ys)
                  | x == head y = (x:y) : ys
                  | otherwise = [x] : acc

encode :: (Eq a)=>[a] -> [(Integer, Maybe a)]
encode = (map f).pack
             where f [] = (0, Nothing)
             	   f x  = (myLength x, Just $ head x)

data Code a = Multiple Int a | Single a | Empty deriving Show
encodeModified :: (Eq a)=>[a] -> [Code a]
encodeModified = (map f).pack
                 where f [] = Empty
             	       f [x]  = Single x
             	       f xs = Multiple  (length xs)  (head xs)

decodeModified :: Eq a=>[Code a]->[a]
decodeModified xs = foldl (++) [] (map f xs)
                 where f (Multiple n x) = replicate n x
                       f (Single x) = [x]
                       f Empty = []

encodeDirect :: Eq a=>[a]->[Code a]
encodeDirect = foldr f []
                  where f x [] = [Single x]
                        f x acc@((Multiple n y):t)
                          | x==y = (Multiple (n+1) x) : t
                          | otherwise = (Single x):acc 
                        f x acc@((Single y):t) = if x==y then (Multiple 2 x) : t
                        	            	           else (Single x):acc

repli :: [a] -> Int -> [a]
repli xs n = foldl (++) [] (map (replicate n) xs)

dropEvery :: [a]->Int->[a]
dropEvery xs n = snd $ foldl f (n,[]) xs
                 where f (1, xs) _ = (n, xs)
                       f (k, xs) x = if k <=0  then error "index out of range"
                       	             else (k-1, xs++[x]) 


