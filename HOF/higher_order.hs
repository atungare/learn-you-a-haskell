myZipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
myZipWith _ [] _ = []
myZipWith _ _ [] = []
myZipWith f (x:xs) (y:ys) = (f x y):(myZipWith f xs ys)

myFlip :: (a -> b -> c) -> b -> a -> c
myFlip f = (\x y -> f y x)

collatz :: (Integral a) => a -> [a]
collatz 1 = [1]
collatz n
  | even n = n:(collatz $ n `div` 2)
  | odd n  = n:(collatz $ 3*n + 1)

mySum :: (Num a) => [a] -> a
mySum = foldl (+) 0

myElem :: (Eq a) => a -> [a] -> Bool
myElem n = foldl (\acc item -> if acc then True else n == item) False

mapUsingFoldr :: (a -> b) -> [a] -> [b]
mapUsingFoldr f = foldr (\item acc -> (f item):acc) []

mapUsingFoldl :: (a -> b) -> [a] -> [b]
mapUsingFoldl f = foldl (\acc item -> acc ++ [f item]) []

myFilter :: (a -> Bool) -> [a] -> [a]
myFilter p = foldr (\item acc -> if (p item) then (item:acc) else acc) []

myReverse :: [a] -> [a]
myReverse = foldl (\acc item -> (item:acc) ) []
-- myReverse = foldl (flip (:)) []

myDot :: (b -> c) -> (a -> b) -> (a -> c)
myDot f g = (\x -> f (g x))