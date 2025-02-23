{-
Just 3 >>= (\x -> Just (x * 2))
📌 Pas cu pas:

Just 3 este o valoare de tip Maybe Int.
(\x -> Just (x * 2)) este o funcție care ia x și returnează Just (x * 2).
>>= extrage valoarea 3, o trimite la funcție și returnează Just 6.
-}



{- Monada Maybe este definita in GHC.Base 

instance Monad Maybe where
  return = Just
  Just va  >>= k   = k va
  Nothing >>= _   = Nothing


instance Applicative Maybe where
  pure = return
  mf <*> ma = do
    f <- mf
    va <- ma
    return (f va)       

instance Functor Maybe where              
  fmap f ma = pure f <*> ma   
-}

------------------------- EX 1 ---------------------------------------
pos :: Int -> Bool
pos  x = if (x>=0) then True else False

fct :: Maybe Int ->  Maybe Bool
fct  mx =  mx  >>= (\x -> Just (pos x))

-- fct :: Maybe Int -> Maybe Bool
-- fct mx = do
--    x <- mx -- extrage valoarea din Maybe (daca exista)
--    return (pos x ) --aplicam pos si punem rezultatul in Just (return in maybe = Just)


--------------------------- EX 2 ---------------------------------------
--sol fara monade
addM :: Maybe Int -> Maybe Int -> Maybe Int
addM Nothing (Just my) = Just my
addM (Just mx) Nothing = Just mx
addM (Just mx) (Just my) = Just (mx+my)

--sol cu monade folosind operatii monadice (>>=)
--folosim >>= pt a extrage valorile din Maybe si a le aduna doar daca ambele sunt Just
--mx >>= (\x -> ...) → dacă mx = Just x, continuăm, altfel returnăm Nothing.
--my >>= (\y -> Just (x + y)) → dacă my = Just y, adunăm x + y, altfel returnăm Nothing.

-- Op >>= extrage valoarea dintr-un context monadic 
--(Maybe, IO, [], etc.) și aplică o funcție asupra acesteia, 
--menținând rezultatul în același context.
addM mx my  = mx >>= (\x->my >>= (\y->Just(x+y)))                                                                                                                                         


-- var cu do-notation
add mx my = do 
    x <- mx -- Extragem x din Maybe Int
    y <- my -- Extragem y din Maybe Int
    return (x+y) --Returnam suma in Just





------------------------------- EX 3 ----------------------------------
cartesian_product xs ys = xs >>= ( \x -> (ys >>= \y-> return (x,y)))

-- cartesian_product :: [a] -> [b] -> [(a, b)]
-- cartesian_product xs ys = do
--     x <- xs
--     y <- ys
--     return (x, y)


prod f xs ys = [f x y | x <- xs, y<-ys]


-- prod :: (a -> b -> c) -> [a] -> [b] -> [c]
-- prod f xs ys = do
--   x <- xs
--   y <- ys
--   return (f x y)

myGetLine :: IO String
myGetLine = getChar >>= \x ->
      if x == '\n' then
          return []
      else
          myGetLine >>= \xs -> return (x:xs)

-- myGetLine :: IO String
-- myGetLine = do
--   x <- getChar
--   if x == '\n' then
--     return []
--   else do
--     xs <- myGetLine
--     return (x:xs)




-------------------------------- EX 4 -------------------------------------------

prelNo noin =  sqrt noin

ioNumber = do
     noin  <- readLn :: IO Float
     putStrLn $ "Intrare\n" ++ (show noin)
     let  noout = prelNo noin
     putStrLn $ "Iesire"
     print noout

-- ioNumber :: IO ()
-- ioNumber =
--   readLn >>= \noin ->
--   putStrLn ("Intrare\n" ++ show noin) >>
--   let noout = prelNo noin in
--   putStrLn "Iesire" >>
--   print noout

------------------------------- EX 6 -------------------------------------

data Person = Person { name :: String, age :: Int }

showPersonN :: Person -> String
showPersonN (Person n _) = "NAME: " ++ n

showPersonA :: Person -> String
showPersonA (Person _ a) = "AGE: " ++ show a


{-
showPersonN $ Person "ada" 20
"NAME: ada"
showPersonA $ Person "ada" 20
"AGE: 20"
-}

showPerson :: Person -> String
showPerson p = "(" ++ showPersonN p ++ ", " ++ showPersonA p ++ ")"


{-
showPerson $ Person "ada" 20
"(NAME: ada, AGE: 20)"
-}


newtype Reader env a = Reader { runReader :: env -> a }


instance Monad (Reader env) where
  return x = Reader (\_ -> x)
  ma >>= k = Reader f
    where f env = let a = runReader ma env
                  in  runReader (k a) env



instance Applicative (Reader env) where
  pure = return
  mf <*> ma = do
    f <- mf
    a <- ma
    return (f a)       

instance Functor (Reader env) where              
  fmap f ma = pure f <*> ma    

import Control.Monad.Reader

mshowPersonN :: Reader Person String
mshowPersonN = do
  p <- ask   -- Obține `Person`
  return $ "NAME: " ++ name p

mshowPersonA :: Reader Person String
mshowPersonA = do
  p <- ask   -- Obține `Person`
  return $ "AGE: " ++ show (age p)


mshowPerson :: Reader Person String
mshowPerson = do
  n <- mshowPersonN
  a <- mshowPersonA
  return $ "(" ++ n ++ ", " ++ a ++ ")"

{-
runReader mshowPersonN  $ Person "ada" 20
"NAME:ada"
runReader mshowPersonA  $ Person "ada" 20
"AGE:20"
runReader mshowPerson  $ Person "ada" 20
"(NAME:ada,AGE:20)"
-}