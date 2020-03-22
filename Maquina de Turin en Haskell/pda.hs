module PDA where

-- Tipo de dato algebraico para simular los estados de un automata.
data State = Q Int deriving (Show, Eq)

-- Sinónimos para símbolos, alfabeto y la funcion de transición.
type Symbol = Char
type Alphabet = [Char]
type Delta = (State, Symbol, Symbol) ->[(State, [Symbol])]

-- Tipo de dato algebraico Automata, implementación de un automáta de pila.
data Automata = PDA {q::[State], s::Alphabet, t::Alphabet, d::Delta, q0::State, z0::Symbol, f::[State]}
-- q : Conjunto de Estados.
-- s : Alfabeto de Entrada.
-- t : Alfabeto de la Pila.
-- d : Función de Transición.
-- q0 : Estado Inicial.
-- z0 : Simbolo Inicial de la Pila.
-- f : Conjunto de Estados Finales.

--Tipo de dato algebraico Stack, implementación de una pila.
data Stack a = Empty | S a (Stack a) deriving Eq
instance (Show a) => Show (Stack a) where
	showsPrec p Empty cad = showsChar '-' cad
	showsPrec p (S x s) cad = shows x (showsChar '|' (shows s cad))

-- Sinonimos para los automatas de pila.
type Machine = (Automata, Stack)
type Config = (State, String, Stack)

-- Funcion que procesa la cadena con configuraciones.
compute :: Machine -> String -> [[Config]]
compute (_,_) "" = []

-- Automata que acepta a L = {a^nb^m^ck | n = m o m = k}
delta1 :: Delta
delta1 (Q 0, '', 'Z') = [(Q 1, "Z"), (Q 4, "Z")]
delta1 (Q 1, 'a', 'A') = [(Q 1, "AA")]
delta1 (Q 1, 'a', 'Z') = [(Q 1, "AZ")]
delta1 (Q 1, '', 'Z') = [(Q 2, "Z")]
delta1 (Q 1, 'b', 'A') = [(Q 2, [])]
delta1 (Q 2, 'b', 'A') = [(Q 2, [])]
delta1 (Q 2, 'c', 'Z') = [(Q 3, "Z")]
delta1 (Q 2, '', 'Z') = [(Q 3, "Z")]
delta1 (Q 3, 'c', 'Z') = [(Q 3, "Z")]
delta1 (Q 4, 'a', 'Z') = [(Q 4, "Z")]
delta1 (Q 4, 'b', 'Z') = [(Q 5, "BZ")]
delta1 (Q 4, '', 'Z') = [(Q 5, "Z")]
delta1 (Q 5, 'b', 'B') = [(Q 5, "BB")]
delta1 (Q 5, 'c', 'B') = [(Q 6, [])]
delta1 (Q 5, '', 'Z') = [(Q 6, "Z")]
delta1 (Q 6, 'c', 'B') = [(Q 6, [])]
delta1 (Q 6, '' , 'Z') = [(Q 7, "Z")]

--Definimos el automata
pda1 = PDA{
	q = [(Q 0), (Q 1), (Q 2), (Q 3), (Q 4), (Q 5), (Q 6)],
	s = ['a', 'b', 'c'],
	t = ['Z', 'A', 'B'],
	d = delta1,
	q0 = (Q 0),
	z0 = 'Z',
	f = [(Q 0), (Q 3), (Q 7)]
}

--************Funciones para el funcionamiento de una pila.************--
-- Función para determinar si una pila es vacia.
esPilaVacia :: Stack a -> Bool
esPilaVacia Empty = True
esPilaVacia p = False

-- Funcion para añadir un elemento a la pila.
push :: a -> Stack a -> Stack a
push x p = S x p

-- Función para sacar un elemento de la pila.
pop :: Stack a -> Stack a
pop Empty = error "No tiene elementos la pila."
pop (P _ p) = p

-- Funcipn para obtener el tope de la pila.
peak :: Stack a -> a
peak Empty = error "La pila esta vacia."
peak (P x _) = x
