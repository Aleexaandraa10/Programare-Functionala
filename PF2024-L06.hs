data Fruct
  = Mar String Bool
  | Portocala String Int

ionatanFaraVierme = Mar "Ionatan" False
goldenCuVierme = Mar "Golden Delicious" True
portocalaSicilia10 = Portocala "Sanguinello" 10
listaFructe = [Mar "Ionatan" False,
                Portocala "Sanguinello" 10,
                Portocala "Valencia" 22,
                Mar "Golden Delicious" True,
                Portocala "Sanguinello" 15,
                Portocala "Moro" 12,
                Portocala "Tarocco" 3,
                Portocala "Moro" 12,
                Portocala "Valencia" 2,
                Mar "Golden Delicious" False,
                Mar "Golden" False,
                Mar "Golden" True]

ePortocalaDeSicilia :: Fruct -> Bool
ePortocalaDeSicilia (Portocala soi _) = soi `elem`  ["Moro", "Tarocco", "Sanguinello"]
ePortocalaDeSicilia _ = False

test_ePortocalaDeSicilia1 =
    ePortocalaDeSicilia (Portocala "Moro" 12) == True
test_ePortocalaDeSicilia2 =
    ePortocalaDeSicilia (Mar "Ionatan" True) == False

nrFeliiSicilia :: [Fruct] -> Int
nrFeliiSicilia lista = sum [x| (Portocala soi x) <-lista, ePortocalaDeSicilia(Portocala soi x)]

test_nrFeliiSicilia = nrFeliiSicilia listaFructe == 52

nrMereViermi :: [Fruct] -> Int
nrMereViermi lista = length[x | (Mar soi x) <-lista, x]

test_nrMereViermi = nrMereViermi listaFructe == 2

--------------------------------------------------------------------

type NumeA = String
type Rasa = String
data Animal = Pisica NumeA | Caine NumeA Rasa
    deriving Show

animal1 = Pisica "Tom"
animal2 = Caine "Rex" "Labrador"
animal3 = Caine "Max" "Bulldog"

vorbeste :: Animal -> String
vorbeste (Pisica _ )= "Meow!"
vorbeste (Caine _ _)= "Woof!"


-- data Maybe a = Nothing | Just a
-- tipul Maybe e o metoda de a trata exceptii, 
-- unde Nothing poate fi tratat ca eroare sau il putem folosi
-- pentru functii, unde un parametru al ei poate fi optional

rasa :: Animal -> Maybe String
rasa (Caine _ rasa) = Just rasa --chiar daca stim ca rasa e string noi tb sa intoarcem un maybe string, deci punem just
rasa (Pisica _) = Nothing


-----------------------------------------------------------
data Linie = L [Int]
   deriving Show
data Matrice = M [Linie]
   deriving Show

verifica :: Matrice -> Int -> Bool
verifica matrice n = foldr (&&) True $ checkLine matrice n
-- $ pune o paranteza in jurul a ce urmeaza dupa el
-- sau verifica matrice n = all $ checkLine matrice n

checkLine::Matrice ->Int ->[Bool]
checkLine (M lines) n = [sum line == n| (L line)<-lines]

test_verif1 = verifica (M[L[1,2,3], L[4,5], L[2,3,6,8], L[8,5,3]]) 10 == False
test_verif2 = verifica (M[L[2,20,3], L[4,21], L[2,3,6,8,6], L[8,5,3,9]]) 25 == True
------------------------------------------------------------

doarPozN :: Matrice -> Int -> Bool
doarPozN matrice n = foldr (&&) True $ filterElem (filterLn matrice n)

filterLn :: Matrice -> Int -> [Linie] 
filterLn (M lines) n = [(L lin) | (L lin) <- lines, length lin == n]

filterElem :: [Linie] -> [Bool]
filterElem lista = [all(>0)vals | (L vals) <- lista]

testPoz1 = doarPozN (M [L[1,2,3], L[4,5], L[2,3,6,8], L[8,5,3]]) 3 == True
testPoz2 = doarPozN (M [L[1,2,-3], L[4,5], L[2,3,6,8], L[8,5,3]]) 3 == False



corect :: Matrice -> Bool
corect (M []) = True
corect (M [x]) = True
corect (M ((L x1):(L x2):xs)) = length x1 == length x2 && corect(M ((L x2):xs))


testcorect1 = corect (M[L[1,2,3], L[4,5], L[2,3,6,8], L[8,5,3]]) == False
testcorect2 = corect (M[L[1,2,3], L[4,5,8], L[3,6,8], L[8,5,3]]) == True
