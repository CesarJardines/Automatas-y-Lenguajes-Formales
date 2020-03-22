import Data.Char
import Data.List
import Data.Maybe
import Data.List.Split

-- 1.- Implementar una máquina de Turing definiendo el tipo de dato algebraico MaqT con base en la definición formal de
--una máquina de Turing estándar.

-- Tipo para los estados de la máquina y direciones.
data State = Q Int deriving (Show, Eq, Ord)
data Dir = Izq | Der | Est deriving (Eq)

--Sinónimos para símbolos, alfabeto, transiciones, Máquina y la función de transición
type Symbol = Char
type Alphabet = [Symbol]
type Trans = ((State, Symbol), (State, Symbol, Dir)) 
type Delta = (State, Symbol) -> Maybe (State, Symbol, Dir) 

type Config = (State , String , Int)

data MaqT = MT {q::[State], e::Alphabet, a::Alphabet, d::Delta, s::Symbol, q0::State, f::[State]}
-- q: conjunto de estados
-- s: alfabeto del lenguaje
-- g: alfabeto de la cinta
-- d: función de transición
-- q0: estado inicial
-- b: símbolo en blanco
-- f: conjunto de estados finales

--2. Definir la función compute que recibe una MaqT, una cadena e imprime
--el procesamiento formal de la cadena con configuraciones.

--Funciones auxiliares para compute

-- Función para pasar una función Delta a la lista de transiciones que representa
listTRA :: Delta -> [State] -> Alphabet -> [Trans]
listTRA d est alfa = foldr (\x xs -> listAux d x xs) [] [(p, sy) | p <- est, sy <- alfa]

daAux :: String -> Int -> Symbol -> String
daAux str i s
    | i < 0 = replicate (-i) s ++ str
    | otherwise = str ++ (replicate i s)  

-- Funcion que nos da como resultado el alfabeto de la cinta para delta (Definicon de MaqT)
sacaAlfabeto :: [Trans] -> Alphabet
sacaAlfabeto d = foldr (\((_, str), (_, nest, _)) xs 
	-> union (union [str] [nest]) xs) 
		[] d

confMaqVer :: MaqT -> Bool
confMaqVer mt  =
    elem (s mt) (a mt) 
    && not (elem (s mt) (e mt)) 
    && elem (q0 mt) (q mt) 
    && aux (e mt) (a mt) 
    && aux (f mt) (q mt)
    && aux (sacaAlfabeto (listTRA (d mt) (q mt) (a mt))  ) (a mt) 
    && aux (sacaEstados (listTRA(d mt) (q mt) (a mt))) (q mt)

-- Escribir el símbolo en la n-ésima posición.
daSymbol :: String -> Int -> Symbol -> Symbol -> (String, Int)
daSymbol str i s sv
    | i < 0 = daSymbol (daAux str i sv) 0 s sv 
    | i >= j = daSymbol (daAux str (i - j + 1) sv) i s sv
    | otherwise = ((take i str) ++ [s] ++ (drop (i+1) str), i) where j = length str

listAux :: Delta -> (State, Symbol) -> [Trans] -> [Trans]
listAux d p's tra = 
	let r = d p's in case r of
            Nothing -> tra 
            Just t -> ((p's, t):tra)


listConf :: MaqT -> [Config] -> [Config]
listConf _ [] = error "Error, la configuracion no es invalida"
listConf mt conf@((estado, str, i):_) =
    let (est, est1, est2) = serchSymbol str i (s mt); 
    	temp = (d mt) (estado, est) in case temp of
        Nothing -> conf 
        Just (estado', ln, d) -> listConf mt ((estado', nnnt, nnnni):conf)
                where nni = case d of -- hay transición, se agrega la
                            Izq -> est2 - 1 -- configuración asociada a la lista y 
                            Der -> est2 + 1 -- se sigue computando
                            Est -> est2
                      ; (nnt, _) = (daSymbol est1 est2 ln (s mt)) -- Escribiendo símbolo en la cinta
                      ; (_, nnnt, nnnni) = serchSymbol  nnt nni (s mt) -- Moviendo la cabeza de la 


-- Obtner el símbolo en la n-ésima posición.
serchSymbol :: String -> Int -> Symbol -> (Symbol, String, Int)
serchSymbol str i sv
    | i < 0 =  serchSymbol (daAux str i sv) 0 sv 
    | i >= j = serchSymbol (daAux str (i - j + 1) sv) i sv
    | otherwise = (str !! i, str, i) where j = length str

-- Funcion donde hago que saque a los estado desde nuestra funcion delta (Definicion de MA)
sacaEstados :: [Trans] -> [State]
sacaEstados d = foldr (\((est, _), (nesta, _, _)) xs 
	-> union (union [est] [nesta]) xs) 
		[] d 

aux :: (Eq z) => [z] -> [z] -> Bool
aux [] _ = True
aux _ [] = False
aux (x:xs) ys
    | elem x ys = aux xs ys
    | otherwise = False


--Termina funciones auxiliares 

compute :: MaqT -> String -> [Config]
compute mt str
    | not (confMaqVer mt) = error "imposible de computar"
    | elem (s mt) str 
    || not (aux str (e mt)) = error "ingresa una cadena valida, vuele a intentar"
    | otherwise = listConf mt [(q0 mt, str, 0)]

--3. Definir la función accept que recibe una MaqT, una cadena y dice
--si la cadena es aceptada por la máquina de Turing. CAAMBIAR ESTA

accept :: MaqT -> String -> Bool
accept maqt c = elem fdk (f maqt) where (fdk, _, _):_ = compute maqt c

--4. Definir la función encode que recibe una MaqT y la codifica para
--pasarla como entrada de la Máquina Universal.

-- Idea La maquina que nos pasan es universal, lo que hay que codificar es delta
-- pedir un nuemro del estado y escribir n+1 veces 
-- :((

--5. Utilizando el tipo de dato algebraico MaqT definir la máquina de Turing
--que acepte el lenguaje L = {a^n b^n c^n } y mostrar la formalización de la cadena aabbcc
maqT :: MaqT
maqT = MT {
    q = [Q 0, Q 1, Q 2, Q 3, Q 4],
    e = ['a', 'b', 'c'],
    a = ['a', 'b', 'c', '_', 'X'],
    d = delta,
    s = '_',
    q0 = Q 0,
    f = [Q 0]
}

-- 1.- Recorremos la cadena para ver que sea de la forma a^n b^n c^n, si no rechazamos
-- 2.- Regresamos la cabeza al inicio de la cadena
-- 3.- Repetimos hasta que la cadena no tenga a's
-- 4.- Sustituimos a por X
-- 5.- Movemos la cabeza hasta leer b
-- 6.- Sustituimos b por X
-- 7.- Mocemos la cabeza hasta leer c
-- 8.- Sustituimos c por X
-- 9.- regresamos al inicio de la cadena
-- 10.- vamos al paso 3
-- 11.- si la cadena contiene b's o c's rechaza 
-- 12. aceota la cadena 

delta :: Delta
delta (Q 0, 'X') = Just (Q 0, 'X', Der)
delta (Q 0, 'a') = Just (Q 1, 'X', Der)
delta (Q 1, 'a') = Just (Q 1, 'a', Der)
delta (Q 1, 'X') = Just (Q 1, 'X', Der)
delta (Q 1, 'b') = Just (Q 2, 'X', Der)
delta (Q 2, 'b') = Just (Q 2, 'b', Der)
delta (Q 2, 'X') = Just (Q 2, 'X', Der)
delta (Q 2, 'c') = Just (Q 3, 'X', Est)
delta (Q 3, 'X') = Just (Q 3, 'X', Izq)
delta (Q 3, 'b') = Just (Q 3, 'b', Izq)
delta (Q 3, 'a') = Just (Q 0, 'a', Est)
delta (Q 3, '_') = Just (Q 4, '_', Der)
delta (Q 4, 'X') = Just (Q 4, 'X', Der)
delta (Q 4, '_') = Just (Q 0, '_', Est)
delta _ = Nothing

















