# Automatas y Lenguajes Formales

Algunas implementaciones de un par de funciones básicas en Haskell, el archivo **Funciones basicas de Haskell**, en cada ReadMe 
se encuentra lo que hace cada funcion de las cuales se encuentran: 

sFibonacci:: Int -> [Int]
```
> sFibonacci 12
[0,1,1,2,3,5,8,13,21,34,55,89,144]
> sFibonacci 15
[0,1,1,2,3,5,8,13,21,34,55,89,144,233,377,610]
```
quitaElemento :: (Eq a ) = > [ a ] -> a -> [ a ]
```
> quitaElemento [1,2,3,4,5,1,2,3,4,5,1,1,1] 1
[2,3,4,5,2,3,4,5]
> quitaElemento [’a’,’a’,’a’,’a’,’a’,’a’] ’a’
[]
```

divisoresPropios :: Int -> [Int]
```
> divisoresPropios 25
[1,5]
> divisoresPropios 1725
[1,3,5,15,23,25,69,75,115,345,575]
```

esPerfecto :: Int -> Bool
```
> esPerfecto 496
True
> esPerfecto 2500
False
```

sonAmigos :: Int -> Int -> Bool
```
> sonAmigos 1184 1210
True
> sonAmigos 114 110
False
```

supersuma :: Int -> Int
```
> supersuma 28
1
> supersuma 3765
3
```

reversar :: [ a ] -> [ a ]
reversal :: [ a ] -> [ a ]

```
> reversar [1,2,3,4,5]
[5,4,3,2,1]
> reversal [3,7,6,5]
[5,6,7,3]

```

En el archivo **pertenencia del lenguaje** se encarga de verificar si dada una cadena de entrada verificar si está se 
encuentra en el Lenguaje haciendo uso de **Regex**. Las funciones que se hacen son:

simpl :: Regex -> Regex
```
> simpl (Star (Add (Symbol ’a’) Void))
a*
> simpl (Concat Epsilon (Add (Symbol ’a’) (Symbol ’b’)))
(a + b)
```

denot :: Regex -> Language
```
> denot (Star (Add (Symbol ’a’) Void))
["","a","aa","aaa","aaaa","aaaaa","aaaaaa","aaaaaaa" ...
> denot (Concat Epsilon (Add (Symbol ’a’) (Symbol ’b’)))
["a","b"]
```

matchD :: String -> Regex -> Bool

```
> matchD "aaaaaaaaa" (Star (Add (Symbol ’a’) Void))
True
> matchD "aba" (Concat Epsilon (Add (Symbol ’a’) (Symbol ’b’)))
False
```

deriv :: String -> Regex -> Regex
```
> deriv "aaaaaaaaa" (Star (Add (Symbol ’a’) Void))
a*
> deriv "aba" (Concat Epsilon (Add (Symbol ’a’) (Symbol ’b’)))
ø
```

matchV :: String -> Regex -> Bool
```
> matchV "aaaaaaaaa" (Star (Add (Symbol ’a’) Void))
True
> matchV "aba" (Concat Epsilon (Add (Symbol ’a’) (Symbol ’b’)))
False
```

En el archivo de **Maquina de Turing** lo que se hace es simular una Máquina de Turing en lenguage Haskell.


















