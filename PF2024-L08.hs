class Collection c where
  empty :: c key value
  singleton :: key -> value -> c key value
  insert
      :: Ord key
      => key -> value -> c key value -> c key value
  clookup :: Ord key => key -> c key value -> Maybe value
  delete :: Ord key => key -> c key value -> c key value
  keys :: c key value -> [key]
  values :: c key value -> [value]
  toList :: c key value -> [(key, value)]
  fromList :: Ord key => [(key,value)] -> c key value


------------------------------------- EX1 --------------------------------------------

keys c = [key | (key, _) <- toList c]
values c = map snd (toList c)
fromList xs = foldr (uncurry insert) empty xs

-------------------------------------- EX 2--------------------------------------

newtype PairList k v
  = PairList { getPairList :: [(k, v)] }

instance Collection PairList where
  empty = PairList[]
  singleton key value = PairList[(key,value)]
  insert key value (PairList xs) = PairList((key, value) : filter (\(key2, value2) -> key2 /= key) xs)
  clookup key collection = lookup key $ getPairList collection   
  delete k (PairList l ) = PairList (filter((/= k).fst ) l)
  toList = getPairList
  
---------------------------------------- EX 3 ----------------------------------------------------------

data SearchTree key value
  = Empty
  | BNode
      (SearchTree key value) -- elemente cu cheia mai mica
      key                    -- cheia elementului
      (Maybe value)          -- valoarea elementului
      (SearchTree key value) -- elemente cu cheia mai mare

instance Collection SearchTree where
  empty = Empty
  singleton k v = BNode Empty k (Just v) Empty 
  insert k v Empty = singleton k v
  insert k v (BNode left key val right)
   | k < key = BNode (insert k v left) key val right 
   | k > key = BNode left key val (insert k v right)
   | otherwise = BNode left key (Just v) right
  clookup _ Empty = Nothing
  clookup k (BNode left key val right)
   | k < key = clookup k left
   | k > key = clookup k right
   | otherwise = val
  delete k Empty = Empty
  delete k (BNode left key val right)
   | k < key = BNode (delete k left) key val right
   | k > key = BNode left key val (delete k right)
   | otherwise = BNode left key Nothing right
  toList Empty = []
  toList (BNode left key Nothing right) = toList left ++ toList right
  toList (BNode left key (Just v) right) = toList left ++ [(key,v)] ++ toList right

------------------------------------------ EX 4 ------------------------------------------------

data Punct = Pt [Int]

instance Show Punct where
  show (Pt list) = "(" ++ parse list ++ ")"
    where
      parse [] = ""
      parse [x] = show x
      parse (x1:x2:xs) = show x1 ++ "," ++ parse(x2:xs)

-- Pt [1,2,3]
-- (1, 2, 3)

-- Pt []
-- ()

---------------------------------------- EX 5 ------------------------------------------------------

data Arb = Vid | F Int | N Arb Arb
          deriving Show

class ToFromArb a where
 	  toArb :: a -> Arb
	  fromArb :: Arb -> a

instance ToFromArb Punct where
    toArb (Pt []) = Vid  
    toArb (Pt (x:xs)) = N (F x) (toArb (Pt xs))
    fromArb Vid = Pt [] 
    fromArb (F x) = Pt [x]
    fromArb (N left right) = let Pt l = fromArb left
                                 Pt r = fromArb right
                              in Pt (l ++ r)


-- toArb (Pt [1,2,3])
-- N (F 1) (N (F 2) (N (F 3) Vid))
-- fromArb $ N (F 1) (N (F 2) (N (F 3) Vid)) :: Punct
--  (1,2,3)

-------------------------------------------- EX 6------------------------------------------------------------------------------

data Geo a = Square a | Rectangle a a | Circle a
    deriving Show

class GeoOps g where
  perimeter :: (Floating a) => g a -> a
  area :: (Floating a) =>  g a -> a

instance GeoOps Geo where
  perimeter (Square a) = 4 * a
  perimeter (Rectangle l w) = 2 * (l + w)
  perimeter (Circle r) = 2 * pi * r
  area (Square a) = a ^ 2
  area (Rectangle l w) = l * w
  area (Circle r) = pi * r ^ 2

-- ghci> pi
-- 3.141592653589793

------------------------------------------ EX 7 -------------------------------------------------------------------
instance (Floating a, Eq a) => Eq (Geo a) where
  g1 == g2 = perimeter g1 == perimeter g2