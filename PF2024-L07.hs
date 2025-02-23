data Expr = Const Int -- integer constant
          | Expr :+: Expr -- , + este op infix
          | Expr :*: Expr -- multiplication
           deriving Eq
-- am fct un tip de date definit de noi
-- Expr este constructor de tip si nu are argument
-- op infix se definesc neaparat cu : :, acel plus va avea structura (const int)+(const int)
-- Eq este o clasa de tipuri cuprinde toate tipurile care implementeaza o fct


data Operation = Add | Mult deriving (Eq, Show)

data Tree = Lf Int -- leaf
          | Node Operation Tree Tree -- branch
           deriving (Eq, Show)
           
instance Show Expr where
  show (Const x) = show x
  show (e1 :+: e2) = "(" ++ show e1 ++ " + "++ show e2 ++ ")"
  show (e1 :*: e2) = "(" ++ show e1 ++ " * "++ show e2 ++ ")"           

evalExp :: Expr -> Int
evalExp (Const a) = a
evalExp (e1 :+: e2) = evalExp e1 + evalExp e2
evalExp (e1 :*: e2) = evalExp e1 * evalExp e2

exp1 = ((Const 2 :*: Const 3) :+: (Const 0 :*: Const 5))
exp2 = (Const 2 :*: (Const 3 :+: Const 4))
exp3 = (Const 4 :+: (Const 3 :*: Const 3))
exp4 = (((Const 1 :*: Const 2) :*: (Const 3 :+: Const 1)) :*: Const 2)
test11 = evalExp exp1 == 6
test12 = evalExp exp2 == 14
test13 = evalExp exp3 == 13
test14 = evalExp exp4 == 16
----------------------------------------------------------

evalArb :: Tree -> Int
evalArb (Lf x) = x
evalArb (Node Mult t1 t2) = evalArb t1 * evalArb t2
evalArb (Node Add t1 t2) = evalArb t1 + evalArb t2


arb1 = Node Add (Node Mult (Lf 2) (Lf 3)) (Node Mult (Lf 0)(Lf 5))
arb2 = Node Mult (Lf 2) (Node Add (Lf 3)(Lf 4))
arb3 = Node Add (Lf 4) (Node Mult (Lf 3)(Lf 3))
arb4 = Node Mult (Node Mult (Node Mult (Lf 1) (Lf 2)) (Node Add (Lf 3)(Lf 1))) (Lf 2)

test21 = evalArb arb1 == 6
test22 = evalArb arb2 == 14
test23 = evalArb arb3 == 13
test24 = evalArb arb4 == 16


expToArb :: Expr -> Tree
expToArb (Const t) = Lf t
expToArb (e1 :+: e2) = Node Add (expToArb e1) (expToArb e2)
expToArb (e1 :*: e2) = Node Mult (expToArb e1) (expToArb e2)

----------------------------------------------------------------------------------

data IntSearchTree value --constructor de tip parametrizat, value este tipul valorilor(int de exemplu)
  = Empty --Empty si Bnode sunt constructori adica fct de constructie
  | BNode
      (IntSearchTree value)     -- elemente cu cheia mai mica, subarbore stang
      Int                       -- cheia elementului
      (Maybe value)             -- valoarea elementului
      (IntSearchTree value)     -- elemente cu cheia mai mare, subarbore drept
  deriving Show
--Maybe e un parametru care poate lipsi
--Maybe a = Nothing|Just a


lookup' :: Int -> IntSearchTree value -> Maybe value
lookup' key Empty = Nothing
lookup' key (BNode left treeKey treeValue right) 
  | treeKey == key = treeValue
  | treeKey < key = lookup' key right
  | treeKey > key = lookup' key left 
--am scris lookup' key left pt ca am fct pattern matching
--haskell deja stie ca al doilea argument al lui lookup' va fi un subarbore

keys ::  IntSearchTree value -> [Int]
keys Empty = []
keys (BNode left treeKey _ right) = keys left ++[treeKey] ++ keys right
-- ++ e op de concatenare


values :: IntSearchTree value -> [value]
values Empty = []
values (BNode left _ Nothing right) = values left ++ values right
values (BNode left _ (Just valuesEl) right) = values left ++[valuesEl]++ values right
-- ai grija la tipul Maybe!!!!!


insert :: Int -> value -> IntSearchTree value -> IntSearchTree value
insert key val Empty =  BNode Empty key (Just val) Empty
insert key val (BNode left treeKey treeVal right) 
  | key == treeKey   = BNode left treeKey (Just val) right
  | key < treeKey    = BNode (insert key val left) treeKey treeVal right
  | key > treeKey    = BNode left treeKey treeVal (insert key val right)


delete :: Int -> IntSearchTree value -> IntSearchTree value
delete key Empty = Empty
delete key (BNode left treeKey treeVal right)
  | key == treeKey = BNode left treeKey Nothing right
  | key > treeKey = BNode left treeKey treeVal (delete key right)
  | key < treeKey = BNode (delete key left) treeKey treeVal right



toList :: IntSearchTree value -> [(Int, value)]
toList Empty =[]
toList (BNode left key Nothing right) = toList left ++ toList right
toList (BNode left key (Just value) right) = toList left ++ [(key, value)] ++toList right
  


fromList :: [(Int,value)] -> IntSearchTree value 
fromList [] = Empty
fromList (h:t) = insert (fst h) (snd h) (fromList t)
--fts si snd reprezinta primul si al doilea el din tuplu curent

fromListFold :: [(Int,value)] -> IntSearchTree value 
fromListFold list =foldr (\(key, val) acc -> insert key val acc) Empty list
-- foldr are 3 argumente, functie, cazul de baza, si de unde iteram
-- (key, val) este primul argument al functiei
-- acc este un rezultat intermediar de tipul rezultatului final pe care ni-l dorim
-- fct noastra adauga o cheie valoarea la un arbore pe care il tot construim intermediar
-- ca sa retin mai usor, acc e ca si cum i-ul dintr-un for
-- for-ul nostru ia valorile din list
-- iar inainte de a incepe forul initializam arborele cu empty



printTree :: IntSearchTree value -> String
printTree Empty = ""
printTree (BNode left treeKey treeVal right) = "(" ++ (printTree left) ++ ")" ++ show treeKey ++ "(" ++ (printTree right) ++ ")"
--fct show primeste o val de tipul repsectiv si intoarce string


-- balance :: IntSearchTree value -> IntSearchTree value
-- balance = undefined
--hint de cautat el median, sa l punem ca root si apoi sa generam stanga si dreapta