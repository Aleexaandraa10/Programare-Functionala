verifL :: [Int] -> Bool
verifL l = (length l) `mod` 2 == 0
-- sau verifL l == even $ length 1


takefinal :: [Int] -> Int -> [Int]
takefinal l n
    |n < length l = drop(length l - n) l
    |otherwise = l 
--in loc sa fac ca la drop 3 [1,2,3,4] ---> [4]
--takefinal va face [1,2,3,4] 3 --> [2,3,4]


remove::[Int] -> Int -> [Int]
remove l n = (take (n-1) l ) ++ (drop n l )
--eliminam el aflat pe poz n




-- semiPareRec [0,2,1,7,8,56,17,18] == [0,1,4,28,9]
semiPareRec :: [Int] -> [Int]
semiPareRec [] = []
semiPareRec (h:t) --head, tail
 | even h    = h `div` 2 : t' --va continua recursivitatea cu restul listei (tailul)
 | otherwise = t'
 where t' = semiPareRec t --unde e t' va inlocui cu semiPareRec take
 --recursivitate aici!!!!!!!!!
 

--exista replicate, dar o implemnetam noi acum
myreplicate :: Int -> Int -> [Int]
myreplicate n v
    | n == 0 = []
    |otherwise = v:(myreplicate (n-1) v)



sumImp::[Int]->Int
sumImp [] = 0
sumImp (h:t)
    | odd h =  sumImp(t) + h
    |otherwise = sumImp(t)



totalLen :: [String] -> Int
totalLen [] = 0
totalLen (('A' : string):t) = 1+length string + totalLen t --pattern matching, daca head e A intra direct aici
totalLen (h:t) = totalLen t
--totalLen (h:t) = if (h !! 0 =='A') then length h + totalLen t else totalLen t



palindrom::String ->Bool
palindrom str = str ==reverse(str)

nrVocale :: [String] -> Int
nrVocale = undefined
-- nrVocale ["sos", "civic", "palton", "desen", "aerisirea"] == 9



-- f 3 [1,2,3,4,5,6] = [1,2,3,3,4,3,5,6,3]

semiPareComp :: [Int] -> [Int]
semiPareComp l = [ x `div` 2 | x <- l, even x ]




----------------------------------------------------------------------------------------
divizori :: Int -> [Int]
divizori n = [divizor| divizor <- [1..n], n `mod` divizor ==0] 
--compresiune ca la python, am selectat toate nr de la 1 la n si am filtrat ca n mod divizor=0
-- divizori 4 == [1,2,4]

--filter ::          (a->Bool) ->                      [a]      ->[a]
divizori' n =filter(\divizor -> n `mod` divizor == 0) [1..n]

divide::Int->Int->Bool
divide n  divizor = n `mod` divizor ==0 --definim bool ul
--divide n = \divizor -> n `mod` divizor ==0 --aici luam doar n si definim o functie prin \






listadiv :: [Int] -> [[Int]]
listadiv l = [divizori n | n <- l]
--selectam toate elem din l si le numim n, pt fiecare elem aplicam divizori definita mai sus
-- listadiv [1,4,6,8] == [[1],[1,2,4],[1,2,3,6],[1,2,4,8]]



inInterval::Int->Int->[Int]->[Int]
inInterval x y l = [n| n<-l, x<=n && n<=y]
--daca am scrie n*10 inainte de | inseamna ca luam nr din [x,y]
--si le afisez inmultite cu 10
-- inInterval 5 10 [1..15] == [5,6,7,8,9,10]
-- inInterval 5 10 [1,3,5,2,8,-1] == [5,8]


inIntervalRec::Int->Int->[Int]->[Int]
inIntervalRec x y [] = []
inIntervalRec x y (h:t) = if (x<=h && h<=y) then h:inIntervalRec x y t
                          else inIntervalRec x y t





pozitive::[Int]->Int
pozitive l = length [n| n<-l, n>0]
-- pozitive [0,1,-3,-2,8,-1,6] == 3


pozitiveRec::[Int]->Int
pozitiveRec [] = 0
pozitiveRec (h:t) = if h>0 then 1+pozitiveRec t
                    else pozitiveRec t





pozitii' :: [Int]->Int->[Int]
pozitii' [] n =[]
pozitii' (h:t) n   
    | even n = pozitii' t (n+1) 
    | otherwise = n:pozitii' t (n+1)


pozitiiImpare :: [Int] ->[Int]
pozitiiImpare l = pozitii' l 0
-- pozitiiImpare [0,1,-3,-2,8,-1,6,1] == [1,2,5,7]


--zip [3,4,99,1][1..] ---> [(3,1),(4,2),(99,3),(1,4)]
--pereche elem, poz

--TINAND CONT DE ZIP ------->

pozitiiImparezip::[Int]->[Int]
pozitiiImparezip l = [ fst x | x <- zip l [0..], odd (snd x)]
--le am filtrat numai pe acelea a caror indicii sunt impari, dar pe noi ne intereseaza valorile

--Tuple: (v1, v2)
--fst (v1, v2) == v1
-- snd(v1, v2) ==v2


multiDigits::String->Int
multiDigits [] = 1
multiDigits (h:t)
--????????????

-- multDigits "The time is 4:25" == 40
-- multDigits "No digits here!" == 1
