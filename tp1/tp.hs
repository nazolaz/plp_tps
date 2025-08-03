module Proceso (Procesador, AT(Nil,Tern), RoseTree(Rose), Trie(TrieNodo), foldAT, foldRose, foldTrie, procVacio, procId, procCola, procHijosRose, procHijosAT, procRaizTrie, procSubTries, unoxuno, sufijos, inorder, preorder, postorder, preorderRose, hojasRose, ramasRose, caminos, palabras, ifProc,(++!), (.!)) where

import Test.HUnit
import Data.Maybe

--Definiciones de tipos

type Procesador a b = a -> [b]


-- Árboles ternarios
data AT a = Nil | Tern a (AT a) (AT a) (AT a) deriving Eq
--E.g., at = Tern 1 (Tern 2 Nil Nil Nil) (Tern 3 Nil Nil Nil) (Tern 4 Nil Nil Nil)
--Es es árbol ternario con 1 en la raíz, y con sus tres hijos 2, 3 y 4.

-- RoseTrees
data RoseTree a = Rose a [RoseTree a] deriving Eq
--E.g., rt = Rose 1 [Rose 2 [], Rose 3 [], Rose 4 [], Rose 5 []] 
--es el RoseTree con 1 en la raíz y 4 hijos (2, 3, 4 y 5)

-- Tries
data Trie a = TrieNodo (Maybe a) [(Char, Trie a)] deriving Eq
-- E.g., t = TrieNodo (Just True) [('a', TrieNodo (Just True) []), ('b', TrieNodo Nothing [('a', TrieNodo (Just True) [('d', TrieNodo Nothing [])])]), ('c', TrieNodo (Just True) [])]
-- es el Trie Bool de que tiene True en la raíz, tres hijos (a, b, y c), y, a su vez, b tiene como hijo a d.


-- Definiciones de Show

instance Show a => Show (RoseTree a) where
    show = showRoseTree 0
      where
        showRoseTree :: Show a => Int -> RoseTree a -> String
        showRoseTree indent (Rose value children) =
            replicate indent ' ' ++ show value ++ "\n" ++
            concatMap (showRoseTree (indent + 2)) children

instance Show a => Show (AT a) where
    show = showAT 0
      where
        showAT :: Show a => Int -> AT a -> String
        showAT _ Nil = replicate 2 ' ' ++ "Nil"
        showAT indent (Tern value left middle right) =
            replicate indent ' ' ++ show value ++ "\n" ++
            showSubtree (indent + 2) left ++
            showSubtree (indent + 2) middle ++
            showSubtree (indent + 2) right
        
        showSubtree :: Show a => Int -> AT a -> String
        showSubtree indent subtree =
            case subtree of
                Nil -> replicate indent ' ' ++ "Nil\n"
                _   -> showAT indent subtree

instance Show a => Show (Trie a) where
    show = showTrie ""
      where 
        showTrie :: Show a => String -> Trie a -> String
        showTrie indent (TrieNodo maybeValue children) =
            let valueLine = case maybeValue of
                                Nothing -> indent ++ "<vacío>\n"
                                Just v  -> indent ++ "Valor: " ++ show v ++ "\n"
                childrenLines = concatMap (\(c, t) -> showTrie (indent ++ "  " ++ [c] ++ ": ") t) children
            in valueLine ++ childrenLines

--Ejercicio 1
procVacio :: Procesador a b
procVacio = const []

procId :: Procesador a a
procId = \x -> [x]

procCola :: Procesador [a] a
procCola = \x -> if null x then [] else tail x

procHijosRose :: Procesador (RoseTree a) (RoseTree a)
procHijosRose = \(Rose _ hijos) -> hijos 

procHijosAT :: Procesador (AT a) (AT a)
procHijosAT at = case at of
                        Nil -> []
                        (Tern _ izq cen der) -> [izq, cen, der]

procRaizTrie :: Procesador (Trie a) (Maybe a)
procRaizTrie = \(TrieNodo val _) -> [val]

procSubTries :: Procesador (Trie a) (Char, Trie a)
procSubTries  = \(TrieNodo _ hijos) -> hijos


--Ejercicio 2

foldAT :: (a -> b -> b -> b -> b) -> b -> AT a -> b
foldAT cTern cNil at = case at of
                            Nil -> cNil
                            Tern raiz izq cen der -> cTern raiz (f izq) (f cen) (f der)
                        where f = foldAT cTern cNil

foldRose :: (a -> [b] -> b) -> RoseTree a -> b 
foldRose cRose (Rose rose hijos) = cRose rose (map rec hijos)
                                where rec = foldRose cRose

foldTrie cTrie (TrieNodo maybe hijos) = cTrie maybe (map rec hijos)
                                where rec = (\(char, trie) -> (char, foldTrie cTrie trie))


--Ejercicio 3
unoxuno :: Procesador [a] [a]
unoxuno = \lista -> foldr f [] lista
    where f = \x rec -> [x] : rec

sufijos :: Procesador [a] [a]
sufijos = \lista -> foldl (\ac x -> (x : (head ac)) : ac) [[]] (reverse lista)


--Ejercicio 4

preorder :: Procesador (AT a) a
preorder = foldAT (\raiz izq cen der -> [raiz] ++ izq ++ cen ++ der) []

inorder :: Procesador (AT a) a
inorder = foldAT (\raiz izq cen der -> izq ++ cen ++ [raiz] ++ der) []

postorder :: Procesador (AT a) a
postorder = foldAT (\raiz izq cen der -> izq ++ cen ++ der ++ [raiz]) []

--Ejercicio 5


preorderRose :: Procesador (RoseTree a) a
preorderRose = foldRose (\rose rec -> rose : concat rec)

hojasRose :: Procesador (RoseTree a) a
hojasRose = foldRose (\rose rec -> if null rec then [rose] else concat rec)

ramasRose :: Procesador (RoseTree a) [a]
ramasRose =  foldRose (\rose rec -> if null rec then [[rose]] else map (rose :) (concat rec))


--Ejercicio 6
caminos :: Procesador (Trie a) [Char]
caminos = foldTrie (\_ rec -> if null rec then [[]] else [] : concatMap (\(char, sufix) -> map (char :) sufix) rec)


--Ejercicio 7

palabras :: Procesador (Trie a) [Char]
palabras = foldTrie (\val rec -> if null rec then if (isNothing val) then [] else [[]] else if (isNothing val) then (concat (camino rec)) else [] : (concat (camino rec))) 
                              where camino = (map (\(char, sufix) -> (map (char : ) sufix)))


--Ejercicio 8
-- 8.a)
ifProc :: (a->Bool) -> Procesador a b -> Procesador a b -> Procesador a b
ifProc ifcond iftrue iffalse = (\x -> if ifcond x then iftrue x else iffalse x)

esNil :: AT a -> Bool
esNil Nil = True
esNil _ = False


-- 8.b)

(++!) :: Procesador a b -> Procesador a b -> Procesador a b
(++!) pri seg = (\x -> pri x ++ seg x)

-- 8.c)
(.!) :: Procesador b c -> Procesador a b -> Procesador a c
(.!) pri seg = (\x -> concat (map pri (seg x)))

--Ejercicio 9
-- Demostracion enviada en el archivo PDF en el zip.

{-Tests-}

main :: IO Counts
main = do runTestTT allTests

allTests = test [ -- Reemplazar los tests de prueba por tests propios
  "ejercicio1" ~: testsEj1,
  "ejercicio2" ~: testsEj2,
  "ejercicio3" ~: testsEj3,
  "ejercicio4" ~: testsEj4,
  "ejercicio5" ~: testsEj5,
  "ejercicio6" ~: testsEj6,
  "ejercicio7" ~: testsEj7,
  "ejercicio8a" ~: testsEj8a,
  "ejercicio8b" ~: testsEj8b,
  "ejercicio8c" ~: testsEj8c
  ]

-- Formato de tests: {expresion que espero recibir al llamar a mi funcion} ~=? {llamado a la funcion con el argumento correspondiente}
-- separados por comas, adentro de la lista 'test'

-- Preguntar como hacer funcar las cosas con listas vacias
testsEj1 = test [ -- Casos de test para el ejercicio 1
  --procVacio:
      ([]::[Int]) ~=? procVacio ([]::[Int]),
      ([]::[Int]) ~=? procVacio 23,
      ([]::[Bool]) ~=? procVacio True,
      ([]::[Int]) ~=? procVacio [1, 2, 3],
      ([]::[Char]) ~=? procVacio "test",
      ([]::[AT Int]) ~=? procVacio (Nil),
      ([]::[AT Int]) ~=? procVacio (Tern 12 Nil Nil (Tern 22 (Tern 9 Nil Nil Nil) (Tern 2 Nil Nil Nil) Nil)),
      ([]::[(RoseTree Int)]) ~=? procVacio (Rose 12 [Rose 22 [Rose 9 [], Rose 2 []]]),
      ([]::[Trie Bool]) ~=? procVacio (TrieNodo (Just True) [('a', TrieNodo (Just True) []), ('t', TrieNodo Nothing [('e', TrieNodo (Just True) [('s', TrieNodo Nothing [('t', TrieNodo (Just True) [])])])]), ('b', TrieNodo (Just False) [])]),

    --procId:
        [23] ~=? procId 23,
        [True] ~=? procId True,
        ([[]]::[[Int]]) ~=? procId ([]::[Int]),
        [[1, 2, 3]] ~=? procId [1, 2, 3],
        ["test"] ~=? procId "test",
        ([Nil]::[AT Int]) ~=? procId Nil,
        [Tern 12 Nil Nil (Tern 22 (Tern 9 Nil Nil Nil) (Tern 2 Nil Nil Nil) Nil)] ~=? procId (Tern 12 Nil Nil (Tern 22 (Tern 9 Nil Nil Nil) (Tern 2 Nil Nil Nil) Nil)),
        [Rose 12 [Rose 22 [Rose 9 [], Rose 2 []]]] ~=? procId (Rose 12 [Rose 22 [Rose 9 [], Rose 2 []]]),
        [TrieNodo (Just True) [('a', TrieNodo (Just True) []), ('t', TrieNodo Nothing [('e', TrieNodo (Just True) [('s', TrieNodo Nothing [('t', TrieNodo (Just True) [])])])]), ('b', TrieNodo (Just False) [])]] ~=? procId (TrieNodo (Just True) [('a', TrieNodo (Just True) []), ('t', TrieNodo Nothing [('e', TrieNodo (Just True) [('s', TrieNodo Nothing [('t', TrieNodo (Just True) [])])])]), ('b', TrieNodo (Just False) [])]),

      --procCola:
        ([]::[Int]) ~=? procCola ([]::[Int]),
        [] ~=? procCola [23],
        [12] ~=? procCola [23, 12],
        [12, 22] ~=? procCola [23, 12, 22],
        [True, True] ~=? procCola [False, True, True],
        "est" ~=? procCola "test",
        [Nil] ~=? procCola [(Tern 12 Nil Nil (Tern 22 (Tern 9 Nil Nil Nil) (Tern 2 Nil Nil Nil) Nil)), Nil],

      --procHijosRose:
        [] ~=? procHijosRose (Rose 12 []),
        [Rose 22 []] ~=? procHijosRose (Rose 12 [Rose 22 []]),
        [Rose 22 [Rose 9 []]] ~=? procHijosRose (Rose 12 [Rose 22 [Rose 9 []]]),
        [Rose 22 [Rose 9 [], Rose 2 []]] ~=? procHijosRose (Rose 12 [Rose 22 [Rose 9 [], Rose 2 []]]),

      --procHijosAT:
        ([]::[AT Int]) ~=? procHijosAT (Nil),
        [Nil, Nil, Nil] ~=? procHijosAT (Tern 12 Nil Nil Nil),
        [Nil, Nil, (Tern 22 Nil Nil Nil)] ~=? procHijosAT (Tern 12 Nil Nil (Tern 22 Nil Nil Nil)),
        [Nil, Nil, (Tern 22 Nil (Tern 2 Nil Nil Nil) Nil)] ~=? procHijosAT (Tern 12 Nil Nil (Tern 22 Nil (Tern 2 Nil Nil Nil) Nil)),
        [Nil, Nil, (Tern 22 (Tern 9 Nil Nil Nil) (Tern 2 Nil Nil Nil) Nil)] ~=? procHijosAT (Tern 12 Nil Nil (Tern 22 (Tern 9 Nil Nil Nil) (Tern 2 Nil Nil Nil) Nil)),

      --procRaizTrie:
        ([Nothing]::[Maybe Int]) ~=? procRaizTrie (TrieNodo Nothing []),
        [Just True] ~=? procRaizTrie (TrieNodo (Just True) []),
        [Just 22] ~=? procRaizTrie (TrieNodo (Just 22) []),
        [Just True] ~=? procRaizTrie (TrieNodo (Just True) [('a', TrieNodo (Just True) []), ('t', TrieNodo Nothing [('e', TrieNodo (Just True) [('s', TrieNodo Nothing [])])])]),
        [Nothing] ~=? procRaizTrie (TrieNodo Nothing [('a', TrieNodo (Just 22) []), ('t', TrieNodo Nothing [('e', TrieNodo (Just 12) [('s', TrieNodo Nothing [])])])]),
        [Just ""] ~=? procRaizTrie (TrieNodo (Just "") [('a', TrieNodo (Just "a") []), ('t', TrieNodo Nothing [('e', TrieNodo (Just "te") [('s', TrieNodo Nothing [])])])]),

      --procSubTries:
        ([]::[(Char, Trie Bool)]) ~=? procSubTries (TrieNodo (Just True) []),
        [('a', TrieNodo (Just True) [])] ~=? procSubTries (TrieNodo (Just True) [('a', TrieNodo (Just True) [])]),
        [('a', TrieNodo (Just True) []), ('t', TrieNodo Nothing [])] ~=? procSubTries (TrieNodo (Just True) [('a', TrieNodo (Just True) []), ('t', TrieNodo Nothing [])]),
        [('a', TrieNodo (Just True) []), ('t', TrieNodo Nothing [('e', TrieNodo (Just True) [])])] ~=? procSubTries (TrieNodo (Just True) [('a', TrieNodo (Just True) []), ('t', TrieNodo Nothing [('e', TrieNodo (Just True) [])])]),
        [('a', TrieNodo (Just True) []), ('t', TrieNodo Nothing [('e', TrieNodo (Just True) [('s', TrieNodo Nothing [])])])] ~=? procSubTries (TrieNodo (Just True) [('a', TrieNodo (Just True) []), ('t', TrieNodo Nothing [('e', TrieNodo (Just True) [('s', TrieNodo Nothing [])])])]),
        [('a', TrieNodo (Just True) []), ('t', TrieNodo Nothing [('e', TrieNodo (Just True) [('s', TrieNodo Nothing [('t', TrieNodo (Just True) [])])])])] ~=? procSubTries (TrieNodo (Just True) [('a', TrieNodo (Just True) []), ('t', TrieNodo Nothing [('e', TrieNodo (Just True) [('s', TrieNodo Nothing [('t', TrieNodo (Just True) [])])])])]),
        [('a', TrieNodo (Just True) []), ('t', TrieNodo Nothing [('e', TrieNodo (Just True) [('s', TrieNodo Nothing [('t', TrieNodo (Just True) [])])])]), ('b', TrieNodo (Just False) [])] ~=? procSubTries (TrieNodo (Just True) [('a', TrieNodo (Just True) []), ('t', TrieNodo Nothing [('e', TrieNodo (Just True) [('s', TrieNodo Nothing [('t', TrieNodo (Just True) [])])])]), ('b', TrieNodo (Just False) [])])

  ]


testsEj2 = test [ -- Casos de test para el ejercicio 2
    [50] ~=? foldAT (\raiz izq cen der -> [raiz + sum(izq) + sum(der) + sum(cen)]) [] (Tern 16 (Tern 1 Nil Nil Nil) (Tern 14 (Tern 0 Nil Nil Nil) (Tern 3 Nil Nil Nil) (Tern 6 Nil Nil Nil)) (Tern 10 Nil Nil Nil)),
    [28] ~=? foldRose (\rose rec -> [rose + sum(concat rec)]) (Rose 1 [Rose 2 [], Rose 3 [Rose 6 [], Rose 7 []], Rose 4 [], Rose 5 []])
  ]

testsEj3 = test [ -- Casos de test para el ejercicio 3
  -- unoxuno
  ([]::[[Int]]) ~=? unoxuno ([]::[Int]),
  ([]::[[Char]]) ~=? unoxuno "",
  [[1]] ~=? unoxuno [1],
  [[3], [1], [4], [1], [5], [9]] ~=? unoxuno  [3,1,4,1,5,9],
  ["P","a","r","a","d","i","g","m","a","s"] ~=? unoxuno "Paradigmas",
  [[True],[False],[False],[True]] ~=? unoxuno [True,False,False,True],

-- sufijos
  ([[]]::[[Int]]) ~=? sufijos ([]::[Int]), 
  [""] ~=? sufijos "",
  ["Plp", "lp", "p", ""] ~=? sufijos "Plp",
  ([[[], []], [[]], []]::[[[Int]]]) ~=? sufijos ([[], []]::[[Int]]),
  [[1,2,3], [2,3], [3], []] ~=? sufijos [1,2,3],
  [[True, False, True], [False, True], [True], []] ~=? sufijos [True, False, True]

  ]

testsEj4 = test [ -- Casos de test para el ejercicio 4
  -- preorder
    ([]::[AT Int]) ~=? preorder Nil,
    [28] ~=? preorder (Tern 28 Nil Nil Nil),
    "full" ~=? preorder (Tern 'f' (Tern 'u' Nil Nil Nil) (Tern 'l' Nil Nil Nil) (Tern 'l' Nil Nil Nil)), 
    [2,3,5,7] ~=? preorder  (Tern 2 (Tern 3 (Tern 5 (Tern 7 Nil Nil Nil) Nil Nil) Nil Nil) Nil Nil),
    [5,5,5] ~=? preorder (Tern 5 Nil (Tern 5 Nil Nil Nil) (Tern 5 Nil Nil Nil)), 
    "Paradigmas" ~=? preorder (Tern 'P' (Tern 'a' Nil (Tern 'r' Nil Nil Nil) Nil) (Tern 'a' (Tern 'd' Nil Nil Nil) (Tern 'i' Nil Nil Nil) (Tern 'g' Nil Nil Nil)) (Tern 'm' (Tern 'a' (Tern 's' Nil Nil Nil) Nil Nil) Nil Nil)),
    
  -- postorder
    ([]::[AT Int]) ~=? postorder Nil,
    [496] ~=? postorder (Tern 496 Nil Nil Nil),
    "ullf" ~=? postorder (Tern 'f' (Tern 'u' Nil Nil Nil) (Tern 'l' Nil Nil Nil) (Tern 'l' Nil Nil Nil)),  
    [8,13,21,34] ~=? postorder (Tern 34 Nil (Tern 21 Nil (Tern 13 Nil (Tern 8 Nil Nil Nil) Nil) Nil) Nil),
    "aaa" ~=? postorder (Tern 'a' Nil (Tern 'a' Nil Nil Nil) (Tern 'a' Nil Nil Nil)),

  -- inorder 
    ([]::[AT Int]) ~=? inorder Nil,
    [8128] ~=? inorder (Tern 8128 Nil Nil Nil),
    "ulfl" ~=? inorder (Tern 'f' (Tern 'u' Nil Nil Nil) (Tern 'l' Nil Nil Nil) (Tern 'l' Nil Nil Nil)),  
    "radiagPsam" ~=? inorder (Tern 'P' (Tern 'a' Nil (Tern 'r' Nil Nil Nil) Nil) (Tern 'a' (Tern 'd' Nil Nil Nil) (Tern 'i' Nil Nil Nil) (Tern 'g' Nil Nil Nil)) (Tern 'm' (Tern 'a' (Tern 's' Nil Nil Nil) Nil Nil) Nil Nil)),
    "ppp" ~=? inorder (Tern 'p' (Tern 'p' Nil Nil Nil) Nil (Tern 'p' Nil Nil Nil)) 

  ]

testsEj5 = test [ -- Casos de test para el ejercicio 5 rt = Rose 1 [Rose 2 [], Rose 3 [Rose 6 [], Rose 7 []], Rose 4 [], Rose 5 []] 
    --preorderRose
      [16,1,9,7,2,14,0,3,6,10,8,5,4] ~=? preorderRose (Rose 16 [Rose 1 [Rose 9 [], Rose 7 [], Rose 2 []], Rose 14 [Rose 0 [], Rose 3 [], Rose 6 []], Rose 10 [Rose 8 [], Rose 5 [], Rose 4 []]]), -- test preorder enunciado at
      [16] ~=? preorderRose (Rose 16 []), -- rose solo con raiz
      [16,1,14,10] ~=? preorderRose (Rose 16 [Rose 1 [], Rose 14 [], Rose 10 []]), -- rose solo con un nivel bajo la raiz
      ["h","o","l","a"] ~=? preorderRose (Rose "h" [Rose "o" [], Rose "l" [], Rose "a" []]), -- rose solo con un nivel bajo la raiz y letras
    
    --hojasRose
      [16] ~=? hojasRose (Rose 16 []), -- rose solo con raiz
      ["o","l","a"] ~=? hojasRose (Rose "h" [Rose "o" [], Rose "l" [], Rose "a" []]), -- rose solo con un nivel bajo la raiz y letras
      [9,7,2,0,3,6,8,5,4] ~=? hojasRose (Rose 16 [Rose 1 [Rose 9 [], Rose 7 [], Rose 2 []], Rose 14 [Rose 0 [], Rose 3 [], Rose 6 []], Rose 10 [Rose 8 [], Rose 5 [], Rose 4 []]]), -- test preorder enunciado at solo hojas

    -- ramasRose
      [[16]] ~=? ramasRose (Rose 16 []), -- rose solo con raiz
      [["h", "o"], ["h", "l"], ["h", "a"]] ~=? ramasRose (Rose "h" [Rose "o" [], Rose "l" [], Rose "a" []]), -- rose solo con un nivel bajo la raiz y letras
      [["a","b","c"], ["a","b","d"], ["a","b","e"], ["a","f","g"], ["a","f","h"], ["a","f","i"], ["a","j","k"], ["a","j","l"], ["a","j","m"]] ~=? ramasRose (Rose "a" [Rose "b" [Rose "c" [], Rose "d" [], Rose "e" []], Rose "f" [Rose "g" [], Rose "h" [], Rose "i" []], Rose "j" [Rose "k" [], Rose "l" [], Rose "m" []]]) -- test preorder enunciado at
  ]

testsEj6 = test [ -- Casos de test para el ejercicio 6
    ["", "a", "b", "ba", "bad", "c"] ~=? caminos (TrieNodo Nothing [('a', TrieNodo (Just True) []), ('b', TrieNodo Nothing [('a', TrieNodo (Just True) [('d', TrieNodo Nothing [])])]), ('c', TrieNodo (Just True) [])]),
    [""] ~=? caminos (TrieNodo Nothing []),
    ["", "a", "ab", "abc"] ~=? caminos (TrieNodo Nothing [('a', TrieNodo (Just True) [('b', TrieNodo (Just True) [('c', TrieNodo (Just True) [])])])]),
    ["", "a", "b", "c"] ~=? caminos (TrieNodo Nothing [('a', TrieNodo (Just True) []),('b', TrieNodo (Just True) []),('c', TrieNodo (Just True) [])])
  ]

testsEj7 = test [ -- Casos de test para el ejercicio 7
    -- palabras
        [] ~=? palabras (TrieNodo Nothing []), --Trie vacio
        ["ab"] ~=? palabras (TrieNodo Nothing [('a', TrieNodo Nothing [('b', TrieNodo (Just True) [])])]), --Trie con una sola palabra
        ["xy","ab"] ~=? palabras (TrieNodo Nothing [('x', TrieNodo Nothing [('y', TrieNodo (Just True) [])]),('a', TrieNodo Nothing [('b', TrieNodo (Just True) [])])]), --Trie sin prefijos comunes
        ["rojo","ojo"] ~=? palabras (TrieNodo Nothing [('r', TrieNodo Nothing [('o', TrieNodo Nothing [('j', TrieNodo Nothing [('o', TrieNodo (Just True) [])])])]),('o', TrieNodo Nothing [('j', TrieNodo Nothing [('o', TrieNodo (Just True) [])])])]),--Trie con prefijos compartidos
        ["abc","abcd"] ~=? palabras (TrieNodo Nothing [('a', TrieNodo Nothing [('b', TrieNodo Nothing [('c', TrieNodo (Just True) [('d', TrieNodo (Just True) [])])])])]), --Subcadenas
        ["a","ba","c"] ~=? palabras (TrieNodo Nothing [('a', TrieNodo (Just True) []), ('b', TrieNodo Nothing [('a', TrieNodo (Just True) [('d', TrieNodo Nothing [])])]),('c', TrieNodo (Just True)[])]) --Enunciado
      
  ]

testsEj8a = test [ 
   ([]::[AT Int]) ~=? ifProc esNil procVacio procId (Nil), -- caso true
   ([1]::[Int]) ~=? ifProc esNil procVacio preorder (Tern 1 Nil Nil Nil) -- caso false  
  ]
testsEj8b = test [ 
  --at = Tern 16 (Tern 1 (Tern 9 Nil Nil Nil) (Tern 7 Nil Nil Nil) (Tern 2 Nil Nil Nil)) (Tern 14 (Tern 0 Nil Nil Nil) (Tern 3 Nil Nil Nil) (Tern 6 Nil Nil Nil)) (Tern 10 (Tern 8 Nil Nil Nil) (Tern 5 Nil Nil Nil) (Tern 4 Nil Nil Nil))
    [9,7,2,1,0,3,6,14,8,5,4,10,16,16,1,9,7,2,14,0,3,6,10,8,5,4] ~=? (postorder ++! preorder) (Tern 16 (Tern 1 (Tern 9 Nil Nil Nil) (Tern 7 Nil Nil Nil) (Tern 2 Nil Nil Nil)) (Tern 14 (Tern 0 Nil Nil Nil) (Tern 3 Nil Nil Nil) (Tern 6 Nil Nil Nil)) (Tern 10 (Tern 8 Nil Nil Nil) (Tern 5 Nil Nil Nil) (Tern 4 Nil Nil Nil))), --Test del enunciado
    [8, -8] ~=? ((\x -> [2*x]) ++! (\x -> [-2 * x])) 4,  --Test varios
    [0, 8] ~=? ((\x -> [x - 4]) ++! (\x -> [x+4])) 4
  ]
testsEj8c = test [ 
   [0,1,2,0,1,2,3,4] ~=? ((\z->[0..z]) .! (map (+1))) [1,3], --Test del enunciado
   [] ~=? ((\x -> [1]) .! (procHijosAT)) Nil, -- Test con lista vacia #No estoy seguro si da bien#
   [1] ~=? ((\x -> [x]) .! (\x -> [length x])) [1], -- Test lista con un elemento
   [4] ~=? ((\x -> [x]) .! (\x -> [length x])) [1,2,3,4], -- Variante del anterior con mas elementos
   [1,1,1] ~=? ((\x -> [1]) .! procHijosAT) (Tern 1 (Tern 2 Nil Nil Nil) (Tern 3 Nil Nil Nil) (Tern 4 Nil Nil Nil)) -- Probando con lista de varios elementos
  ]