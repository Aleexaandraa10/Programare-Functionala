{-

[x^2 |x <- [1..10], x `rem` 3 == 2] --> [4,25,64]
-mod este întotdeauna pozitiv sau zero, urmând semnul divizorului.
-rem poate fi pozitiv sau negativ, urmând semnul deîmpărțitului.

[(x,y) | x <- [1..5], y <- [x..(x+2)]]
--> [(1,1),(1,2),(1,3),(2,2),(2,3),(2,4),(3,3),(3,4),(3,5),(4,4),(4,5),(4,6),(5,5),(5,6),(5,7)]

[(x,y) | x <- [1..3], let k = x^2, y <- [1..k]]
--> [(1,1),(2,1),(2,2),(2,3),(2,4),(3,1),(3,2),(3,3),(3,4),(3,5),(3,6),(3,7),(3,8),(3,9)]


[x | x <- "Facultatea de Matematica si Informatica", elem x ['A'..'Z']]
--elem x verifica daca este o litera mare si daca se afla intre 'A' si 'Z'
-->FMI

[[x..y] | x <- [1..5], y <- [1..5], x < y]
-->[[1,2],[1,2,3],[1,2,3,4],[1,2,3,4,5],[2,3],[2,3,4],[2,3,4,5],[3,4],[3,4,5],[4,5]]


-}

factori :: Int -> [Int]
factori  n = [ div | div <- [1..n], n `mod` div == 0]

prim :: Int -> Bool
prim n = length ( factori n ) == 2


numerePrime :: Int -> [Int]
numerePrime n = [ nr | nr <- [2..n], prim nr == True]


--[(x,y)| x <- [1..5], y <- [1..3]] --> produsul cartezian
-- > [(1,1),(1,2),(1,3),(2,1),(2,2),(2,3),(3,1),(3,2),(3,3),(4,1),(4,2),(4,3),(5,1),(5,2),(5,3)]

-- zip [1..5] [1..3]
-- > [(1,1),(2,2),(3,3)], perechile de pe ac pozitii

myzip3 :: [Int] -> [Int] -> [Int] -> [(Int, Int, Int)]
myzip3 _ [] _ = []
myzip3 _ _ [] = []
myzip3 [] _ _ = []
myzip3 (h1:t1) (h2:t2) (h3:t3) = (h1, h2, h3) : myzip3 t1 t2 t3

--lambda expresie
--myzip3 (h1:l1) (h2:l2)  = \(h3:l3) -> (h1,h2,h3) : myzip3 l1 l2 l3

--map aplică o funcție pe fiecare element dintr-o listă și returnează o listă cu rezultatele.
-- map (\x-> 2 * x) [1..10]
-- > [2,4,6,8,10,12,14,16,18,20]


--map (1 `elem`) [[2,3], [1,2]]
-- > [False,True]

--  map (`elem` [2,3]) [1,3,4,5]
-- > [False,True,False,False]




firstEl :: [(a, b)] -> [a]
firstEl  = map fst 

sumList :: [[Int]] -> [Int]
sumList liste = map sum liste 
--map aplică funcția sum pe fiecare element din liste.


pre12 :: [Int] -> [Int]
pre12 lista = map transformare lista where 
    transformare x = if even x then x `div` 2 else x*2


contineCaracter :: Char -> [String] -> [String]
contineCaracter ch lista = filter (ch `elem`) lista

patrate :: [Int] -> [Int]
patrate lista = map (^2) (filter odd lista)

patratePoz :: [Int] -> [Int]
patratePoz lista =  map ((^2).fst) (filter (odd.snd) (zip lista [1..]))


numaiVocale :: [String] -> [String]
numaiVocale lista = map transformare lista where
    transformare cuv = filter (`elem` "aeiouAEIOU") cuv

--[[Char]] == [String]
--[String] = lista de cuvinte ( adica lista de siruri de caractere)
--transformare :: [Char] -> [Char]
