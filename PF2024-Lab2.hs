myInt = 31415926535897932384626433832795028841971693993751058209749445923

double :: Integer -> Integer
double x = x+x

--maxim :: Integer -> Integer -> Integer
maxim x y = if (x > y)
               then x
          else y

maxim3 :: Integer -> Integer -> Integer -> Integer
maxim3 x y z = if(x>=y)
                then if(x>=z)
                    then x
                    else z
                else
                    if(y<=z)
                        then z
                        else y


--max3test :: Integer -> Integer -> Integer -> Bool
--nu e nevoie sa scriem asta, se prinde huskelul
max3test x y z = let
                    u=maxim3 x y z
                 in (u>=x && u>=y && u>=z)
                 

max3 x y z = let
             u = maxim x y 
             --u e doar o notatie, nu i se atribuie nimic
             in (maxim  u z)

data Bool232 = True232 | False232 --constructor de date
--True232 il putem lua ca o functie cu 0 valori

and232 :: Bool232 -> Bool232 -> Bool232
and232 _ False232 = False232 --orice boolean cu un fals va intoarce fals
and232 False232 _ = False232 --o sa mearga pe ramjura asta cand primul argument e false
and232 _ _       = True232 --daca nu e nicio situatie de mai sus va merge pe ramura asta
--mai sus e o scriere echivalenta ca la if si else
--_ le am pus in locul unor variabile b1 b2 sa zicem 


suma:: Integer -> Integer -> Integer
suma x y = x^2 + y^2

verifpar :: Integer -> String
verifpar x = if even x then "Par" else "Impar"
-- in loc de even x puteam sa pun `mod` x 2 ==0 sau mod x 2==0

factorial::Integer -> Integer
factorial x = if x==0 then 1 else x*factorial(x-1)

fact2::Integer -> Integer
fact2 0 = 1
fact2 x = x*fact2(x-1)

fact3 :: Integer -> Integer
fact3 x
    |x==0  =1
    |otherwise = fact3(x-1)*x
             
verifpropr x y = if (x>2*y) then "Bun" else "Nu e bun"

get_max :: [Integer] -> Integer
get_max [elem] = elem
get_max (head:tail) = maxim head (get_max tail) --ia max dintre cap si tail
--sau get_max (head:tail) = maxim head $ get_max tail

poly :: Double -> Double -> Double -> Double -> Double
poly a b c x = a*x^2 + b*x + c


eeny :: Integer -> String
eeny = undefined


fizzbuzz :: Integer -> String
fizzbuzz  x = if ( x `mod` 15 ==0)
                then "FizzBuzz"
              else if (x `mod` 3 ==0)
                    then "Fizz"
                  else if (x `mod` 5 ==0)
                    then "Buzz"
                else ""


fizzbuzz1 :: Integer -> String
fizzbuzz1 x  
    |x `mod` 15 == 0 = "FizzBuzz"
    |x `mod` 3 ==0 ="Fizz"
    |x `mod` 5 ==0 ="Buzz"
    |otherwise = "None"





fibonacciCazuri :: Integer -> Integer
fibonacciCazuri n
    | n < 2     = n
    | otherwise = fibonacciCazuri (n - 1) + fibonacciCazuri (n - 2)
    
fibonacciEcuational :: Integer -> Integer
fibonacciEcuational 0 = 0
fibonacciEcuational 1 = 1
fibonacciEcuational n =
    fibonacciEcuational (n - 1) + fibonacciEcuational (n - 2)
    
tribonacci :: Integer -> Integer
tribonacci  x
    |x==1 = 1
    |x==2 =1
    |x==3 =2
    |otherwise = tribonacci(x-1)+tribonacci(x-2)+tribonacci(x-3)

tribonacci1 1 = 1
tribonacci1 2 = 1
tribonacci1 3 = 1
tribonacci1 x = tribonacci1(x-1)+tribonacci1(x-2)+tribonacci1(x-3)


binomial :: Integer -> Integer -> Integer --gards
binomial n k
    |k==0 = 1
    |n==0 = 0
    |otherwise = binomial (n-1) k + binomial (n-1) (k-1)


binomial1 n 0 = 1 --pattern matching
binomial1 0 k =0
binomial1 n k = binomial1 (n-1) k + binomial1 (n-1) (k-1)