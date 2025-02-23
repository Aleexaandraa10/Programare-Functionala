sumImpare::[Int] -> Int
sumImpare xs = foldr (+) 0 (map (^2) (filter odd xs))

verifTrue::[ Bool]-> Bool
verifTrue xs = foldr (&&) True xs


allVerifies :: (Int -> Bool) -> [Int] -> Bool
allVerifies propr xs = foldr (\x aux -> propr x && aux) True xs

anyVerifies :: (Int -> Bool) -> [Int] -> Bool
anyVerifies propr xs = foldr (\x aux -> propr x || aux) False xs

mapFoldr :: (a->b) -> [a] -> [b]
mapFoldr f = foldr (\x acc -> f x : acc) []

--mymap::(a->b) -> [a] -> [b]
--mymap f l = foldr ( \(x:xs) acc -> ((f x):acc)) [] l

filterFoldr :: (a->Bool) -> [a] -> [a]
filterFoldr p l = foldr f e l
    where
        --f :: a -> [a] -> [a]
        f x acc = if p x then x:acc else acc 
        -- e :: [a]
        e = []


listToInt :: [Integer]-> Integer
listToInt lista  = foldl f e lista
    where
        f acc x = acc*10 + x --pt ca e left mai intai vom avea acumulatorul apoi el curent din lista
        e = 0 --cazul de baza


rmChar :: Char -> String -> String
rmChar c string  = filter (/=c) string


rmCharsRec :: String -> String -> String
rmCharRec [] t = t
rmCharsRec (h:t) s = rmCharRec t (rmChar h s)

rmCharsFold :: String -> String -> String
rmCharsFold str1 str2  = foldr f e str2
    where
        --acc e un string, un rezultat intermediar
        f c2 acc = rmChar c2 acc 
        e = str1 

rmCharsFold1 str1 str2 = foldr rmChar str2 str1


myReverse :: [a] -> [a]
myReverse list = foldr f e list
    where
        f x acc = acc ++ [x]
        e =[]

myReverse1 :: [a] -> [a]
myReverse1 list = foldl f e list
    where
        f acc x = x:acc
        e =[]

myUnzip :: [(a, b)] -> ([a], [b])
myUnzip = undefined
