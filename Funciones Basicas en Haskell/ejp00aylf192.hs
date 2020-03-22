--author César Eduardo Jardnies Mendoza
-- Facultad de CIENCIAS, UNAM

-- 1.- FunciónFibonacci que recibe un entero n y 
--regresa una lista con los primeros n +1 elementos de la sucesuón de fibonacci
--Funcion auxiliar que ayuda a sacar el fibonacci del n que le pasen
fib :: Int -> Int
fib 0 = 0
fib 1 = 1
fib n = fib (n-1) + fib (n-2)

sFibonacci :: Int -> [Int]
sFibonacci n = take (n+1) (map fib [0..])

-- 2.- Función quita elemento, recibe una lista de elementos comparables y un elemento de la 
-- lista y regresa una nueva lista sin ninguna aparacion de ese elemento 
quitaElemento :: (Eq a) => [a] -> a -> [a]
quitaElemento [] _ = []
quitaElemento (x:xs) a = if(x /= a) then [x] ++ quitaElemento xs a else quitaElemento xs a

-- 3.- Definir usando listas por comprensión la función divisoresPropios que recibe un entero y regresa una lista con los
--divisores propios de éste. Los divisores propios de un número n son todos los enteros distintos de n que son divisores
--de n.
--Función auxiliar 
divAux:: Int -> Int -> [Int]
divAux n m
	| n == m = []
	| n > m && (n `mod` m == 0) = m:(divAux n (m+1))
	| n > m && (n `mod` m /= 0) = divAux n (m+1)

divisoresPropios:: Int -> [Int]
divisoresPropios n = divAux n 1

--4.- Un número perfecto es un número natural que es igual a la suma de sus divisores propios positivos. Por ejemplo, 6
--es un número perfecto porque sus divisores propios son 1, 2 y 3; y 6 = 1 + 2 + 3. Definir la función esPerfecto
--que reciba un entero positivo y diga si es perfecto o no.
--Función auxiliar 
auxSumaLista :: [Int] -> Int
auxSumaLista [] = 0
auxSumaLista (x:xs) = x + auxSumaLista xs

esPerfecto :: Int -> Bool
esPerfecto n = if (n == auxSumaLista(divAux n 1)) then True else False

--5.- Dos números amigos son dos números enteros positivos a y b tales que la suma de los divisores propios de uno es
--igual al otro número y viceversa, esto es, si sumaDP es función que suma los divisores propios de un número, entonces
--se cumple que sumaDP(a) = b y sumaDP(b) = a. Por ejemplo 220 y 284 son amigos pues sumaDP(220) = 284 y
--sumaDP(284) = 220. Definir la función sonAmigos que recibe dos enteros positivos y determina si son amigos.
sonAmigos :: Int -> Int -> Bool
sonAmigos n m = if (auxSumaLista(divisoresPropios n) == m) && (auxSumaLista(divisoresPropios m) == n) then True else False

--6.- Definir la función supersuma que recibe un entero y regresa la suma de sus dígitos hasta que quede un número de
--un solo dígito.
auxSuperSuma :: Int -> Int
auxSuperSuma 0 = 0
auxSuperSuma n = if(n<10) then n else n `mod` 10 + auxSuperSuma (n `div` 10)

supersuma:: Int -> Int
supersuma n = if (n < 10) then n else supersuma(auxSuperSuma n)

--7. Definir las funciones reversar y reversal que obtienen la reversa de una lista usando las funciones de orden superior
--foldr y foldl respectivamente. Para este ejercicio no se pueden definir funciones auxiliares, es necesario el uso de
--lambdas.
reversar :: [ a ] -> [ a ]
reversar [] = []
reversar bs = foldr (\b g x -> g (b : x)) id bs []

reversal :: [a] -> [a]
reversal [] = []
reversal xs = foldl (\acc x-> x : acc) [] xs






