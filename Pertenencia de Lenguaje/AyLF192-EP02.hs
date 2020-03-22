--César Eduardo Jardines Mendoza
-- Facultad de Ciencias, UNAM
module Regex where

 import Data.List

 -- Tipo de dato algebraico para representar Expresiones Regulares
 data Regex = Void
            | Epsilon
            | Symbol Char           -- El símbolo representado por el caracter que recibe
            | Star Regex            -- r*
            | Concat Regex Regex    -- (rs)
            | Add Regex Regex       -- (r + s)
            deriving (Eq)

 -- Sinónimo para representar lenguajes como listas de cadenas.
 type Language = [String]

 -- Instancia de Show del tipo Regex, para que se impriman con formato en la consola. 
 instance Show Regex where
   show Void = "ø"
   show Epsilon = "ε"
   show (Symbol c) = c:[]
   show (Star r) = show r ++ "*"
   show (Concat r s) = "(" ++ show r ++ show s ++ ")"
   show (Add r s) = "(" ++ show r ++ " + " ++ show s ++ ")"

  ------------------- DENOTACIÓN -----------------------

 -- EJERCICIO 1
 simpl :: Regex -> Regex
 simpl (Symbol r) = (Symbol r)

 simpl (Add Void r) =  simpl r
 simpl (Add r Void) =  simpl r
 simpl (Add Epsilon (Concat (Star a) b)) = if(a == b) then (Star (simpl a)) else (Add Epsilon (Concat (Star (simpl a)) (simpl b)))
 simpl (Add (Concat (Star a) b) Epsilon) = if(a == b) then (Star (simpl a)) else (Add Epsilon (Concat (Star (simpl a)) (simpl b)))
 simpl (Add Epsilon (Concat a1 (Concat (Star (Concat b2 a2)) b1))) = if(a1 == a2 && b1 == b2) then Star (Concat (simpl a1) (simpl b1)) else (Add Epsilon (Concat (simpl a1) (Concat (Star (Concat (simpl b2) (simpl a2))) (simpl b1))))
 simpl (Add (Concat a1 (Concat (Star (Concat b2 a2)) b1)) Epsilon) = if(a1 == a2 && b1 == b2) then Star (Concat (simpl a1) (simpl b1)) else (Add Epsilon (Concat (simpl a1) (Concat (Star (Concat (simpl b2) (simpl a2))) (simpl b1))))
 simpl (Add Epsilon r) = (Add Epsilon (simpl r))
 simpl (Add r Epsilon ) = (Add (simpl r) Epsilon)
 simpl (Add r s) = (Add (simpl r) (simpl s))

 simpl (Concat Epsilon r) = simpl r
 simpl (Concat r Epsilon) = simpl r
 simpl (Concat Void r) = Void
 simpl (Concat r Void) = Void
 simpl (Concat r s) = (Concat (simpl r) (simpl s))

 simpl (Star Void) = Epsilon
 simpl (Star Epsilon) = Epsilon
 simpl (Star (Add Epsilon a)) = simpl (Star a)
 simpl (Star (Add a Epsilon)) = simpl (Star a)
 simpl (Star (Star a)) = simpl (Star a)
 simpl (Star a) = Star (simpl a)

 -- EJERCICIO 2
 denot :: Regex -> Language
 denot Epsilon = [""]
 denot Void = []
 denot (Symbol r) = [show (Symbol r)]
 denot (Add r s) = (denot r) ++ (denot s)
 denot (Concat r s) = [x ++ y | x <- (denot r), y <- (denot s)]
 denot (Star s) = [""] `union` ((denot s) `union` (denot (Concat s (Star s))))

 -- EJERCICIO 3
 matchD :: String -> Regex -> Bool
 matchD l r = if(l `notElem` (denot(simpl r))) then False else True

 -------------------- DERIVADA ----------------------

 -- EJERCICIO 1
 deriv :: String -> Regex -> Regex
 deriv a r = if(a == "") then r else (simpl (deriv (tail a) (derivSym (head a) (simpl r))))

 -- EJERCICIO 2
 --matchV :: String -> Regex -> Bool

 

 nulidad :: Regex -> Regex
 nulidad Epsilon = Epsilon
 nulidad Void = Void
 nulidad (Symbol r) = Void
 nulidad (Add r s) = (Add (nulidad r) (nulidad s))
 nulidad (Concat r s) = (Concat (nulidad r) (nulidad s))
 nulidad (Star s) = Epsilon

 derivSym :: Char ->  Regex -> Regex
 derivSym a Void = Void
 derivSym a Epsilon = Void
 derivSym a (Symbol b) = if(a == b) then Epsilon else Void
 derivSym a (Add r s) = (Add (derivSym a r) (derivSym a s))
 derivSym a (Concat r s) = (Add (Concat (derivSym a r) (s)) (Concat (nulidad r) (derivSym a s)))
 derivSym a (Star s) = (Concat (derivSym a s) (Star s))