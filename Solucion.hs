module Solucion where

import Data.List
import Test.HUnit

type Texto = String
type Feature = Float
type Instancia = [Feature]
type Extractor = (Texto -> Feature)

type Datos = [Instancia]
type Etiqueta = String
type Modelo = (Instancia -> Etiqueta)
type Medida = (Instancia -> Instancia -> Float)

tryClassifier :: [Texto] -> [Etiqueta] -> Float
tryClassifier x y = let xs = extraerFeatures ([longitudPromedioPalabras, repeticionesPromedio] ++ frecuenciaTokens) x in
    nFoldCrossValidation 5 xs y

mean :: [Float] -> Float
mean xs = realToFrac (sum xs) / genericLength xs

--Tests ej1
test_ej1_1 = TestCase (assertEqual "split ',' \"hola PLP, bienvenidos!\"" (["hola PLP"," bienvenidos!"]) (split ',' "hola PLP, bienvenidos!"))
test_ej1_2 = TestCase (assertEqual "split ' ' \" hola PLP, bienvenidos!\"" (["hola", "PLP,", "bienvenidos!"]) (split ' ' " hola PLP, bienvenidos!"))
test_ej1_3 = TestCase (assertEqual "split ' ' \"hola PLP, bienvenidos! \"" (["hola", "PLP,", "bienvenidos!"]) (split ' ' "hola PLP, bienvenidos! "))
test_ej1_4 = TestCase (assertEqual "split ' ' \"  hola PLP, bienvenidos!  \"" (["hola", "PLP,", "bienvenidos!"]) (split ' ' "  hola PLP, bienvenidos!  "))
test_ej1_5 = TestCase (assertEqual "split ' ' \"hola  PLP,   bienvenidos!\"" (["hola", "PLP,", "bienvenidos!"]) (split ' ' "hola  PLP,   bienvenidos!"))
test_ej1_6 = TestCase (assertEqual "split ' ' \"   hola  PLP,   bienvenidos!    \"" (["hola", "PLP,", "bienvenidos!"]) (split ' ' "   hola  PLP,   bienvenidos!    "))
test_ej1_7 = TestCase (assertEqual "split ' ' \"   \"" ([]) (split ' ' "   "))
test_ej1_8 = TestCase (assertEqual "split ' ' \"\"" ([]) (split ' ' ""))
test_ej1_9 = TestCase (assertEqual "split 'a' \"   \"" (["   "]) (split 'a' "   "))
test_ej1_10 = TestCase (assertEqual "split 'a' \"\"" ([]) (split 'a' ""))
test_ej1_11 = TestCase (assertEqual "split \"ab\" [\"ab\", \" ab \",\"ab\", \"c d\", \"cc d\",\"ab\"]" ([[" ab "],["c d", "cc d"]]) (split "ab" ["ab", " ab ","ab", "c d", "cc d","ab"]))
test_ej1_12 = TestCase (assertEqual "split 3 [3,3,2,2,3,1,3,3,3,2,3,3,3]" ([[2,2],[1],[2]]) (split 3 [3,3,2,2,3,1,3,3,3,2,3,3,3]))

tests_ej1 = TestList [TestLabel "test_ej1_1" test_ej1_1,TestLabel "test_ej1_2" test_ej1_2,TestLabel "test_ej1_3" test_ej1_3,TestLabel "test_ej1_4" test_ej1_4,
                      TestLabel "test_ej1_5" test_ej1_5,TestLabel "test_ej1_6" test_ej1_6,TestLabel "test_ej1_7" test_ej1_7,TestLabel "test_ej1_8" test_ej1_8,
                      TestLabel "test_ej1_9" test_ej1_9,TestLabel "test_ej1_10" test_ej1_10, TestLabel "test_ej1_11" test_ej1_11,TestLabel "test_ej1_12" test_ej1_12]

--TODO
--El separador es siempre un char o puede ser otra cosa?
split :: Eq a => a -> [a] -> [[a]]
split a = filter (not.null) . (foldr (\x (z:zs) -> if (x==a) then
                                                     if(null z) then
                                                       z:zs
                                                     else
                                                       []:(z:zs)
                                                    else
                                                     (x:z):zs
                                      ) [[]]
                               )

--Tests ej2
test_ej2_1 = TestCase (assertEqual "longitudPromedioPalabras \"Este test tiene palabras $$++$$\"" (5.4)
                                   (longitudPromedioPalabras "Este test tiene palabras $$++$$"))
test_ej2_2 = TestCase (assertEqual "longitudPromedioPalabras \"  12345    12  \"" (3.5)
                                   (longitudPromedioPalabras "  12345    12  "))
test_ej2_3 = TestCase (assertEqual "longitudPromedioPalabras \"Este\"" (4)
                                   (longitudPromedioPalabras "Este"))
--TODO ¿Que hacer cuando pido longitudPromedioPalabras de una cadena vacia?
--test_ej2_4 = TestCase (assertEqual "longitudPromedioPalabras \"\"" (0)
--                                   (longitudPromedioPalabras ""))

tests_ej2 = TestList [TestLabel "test_ej2_1" test_ej2_1,TestLabel "test_ej2_2" test_ej2_2,TestLabel "test_ej2_3" test_ej2_3
                      --,TestLabel "test_ej2_4" test_ej2_4
                      ]

longitudPromedioPalabras :: Extractor
longitudPromedioPalabras xs = mean $ map genericLength palabras
                               where palabras = split ' ' xs
--longitudPromedioPalabras xs = sumaLongitudes(split ' ' xs) / (genericLength (split ' ' xs))
--sumaLongitudes = foldr (\x sumador -> genericLength x + sumador) 0

--Tests ej3
test_ej3_1 = TestCase (assertEqual "cuentas [\"x\", \"x\", \"y\", \"x\", \"z\"]" [(3,"x"),(1,"y"),(1,"z")]
                                   (cuentas ["x", "x", "y", "x", "z"]))
test_ej3_2 = TestCase (assertEqual "cuentas []" ([]::[(Int, String)])
                                   (cuentas []))
test_ej3_3 = TestCase (assertEqual "cuentas [\"x\"]" [(1,"x")]
                                   (cuentas ["x"]))
test_ej3_4 = TestCase (assertEqual "cuentas [\"x\", \"x\", \"yy\", \"y\", \"z\", \"x\"]" [(3,"x"), (1,"yy"),(1,"y"),(1,"z")]
                                   (cuentas ["x", "x", "yy", "y", "z", "x"]))

tests_ej3 = TestList [TestLabel "test_ej3_1" test_ej3_1,TestLabel "test_ej3_2" test_ej3_2,TestLabel "test_ej3_3" test_ej3_3,TestLabel "test_ej3_4" test_ej3_4]


cuentas :: Eq a => [a] -> [(Int, a)]
--cuentas xs = cantidadDeApariciones (elementosSinRepetir xs) xs
cuentas xs = let sinRepetidos = reverse $ elementosSinRepetir $ reverse xs in
               map (\x -> ((repeticiones x xs), x)) sinRepetidos

               
--TODO
--Preguntar que tiene que hacer el ejercicio 4
--Nosotros lo que hicimos fue dividir la cantidad total de palabras por la cantidad de palabras distintas. Esta bien eso?
--Tests ej4
test_ej4_1 = TestCase (assertEqual "repeticionesPromedio \"lalala $$++$$ lalala lalala $$++$$\"]" (2.5)
                                   (repeticionesPromedio "lalala $$++$$ lalala lalala $$++$$"))
test_ej4_2 = TestCase (assertEqual "repeticionesPromedio \"  aa   x y aa \"]" (1.3333333333)
                                   (repeticionesPromedio "  aa   x y aa "))
test_ej4_3 = TestCase (assertEqual "repeticionesPromedio \"aa aa\"]" (2)
                                   (repeticionesPromedio "aa aa"))
test_ej4_4 = TestCase (assertEqual "repeticionesPromedio \"aaa\"]" (1)
                                   (repeticionesPromedio "aaa"))
--test_ej4_5 = TestCase (assertEqual "repeticionesPromedio \"   \"]" (0)
--                                   (repeticionesPromedio "   "))
--test_ej4_6 = TestCase (assertEqual "repeticionesPromedio \"\"]" (0)
--                                   (repeticionesPromedio ""))

tests_ej4 = TestList [TestLabel "test_ej4_1" test_ej4_1,TestLabel "test_ej4_2" test_ej4_2,TestLabel "test_ej4_3" test_ej4_3,TestLabel "test_ej4_4" test_ej4_4
                      --,TestLabel "test_ej4_5" test_ej4_5, TestLabel "test_ej4_6" test_ej4_6
                     ]

--TODO
--Que hacer si la cadena que le pasan es vacia ("")
repeticionesPromedio :: Extractor
--repeticionesPromedio xs = (fromIntegral $ length $ split ' ' xs) / (fromIntegral $ length $ elementosSinRepetir $ split ' ' xs)
repeticionesPromedio xs = let palabras = split ' ' xs in
                            (fromIntegral $ length palabras) / (fromIntegral $ length $ elementosSinRepetir palabras)

tokens :: [Char]
tokens = "_,)(*;-=>/.{}\"&:+#[]<|%!\'@?~^$` abcdefghijklmnopqrstuvwxyz0123456789"

--Tests ej5
test_ej5_1 = TestCase (assertEqual "(head frecuenciaTokens) \"use_snake_case !\"" (0.125)
                                   ((head frecuenciaTokens) "use_snake_case !"))
--TODO
--Que hacer si la cadena que le pasan es vacia ("")
test_ej5_2 = TestCase (assertEqual "(head frecuenciaTokens) \"\"" (0)
                                   ((head frecuenciaTokens) ""))
test_ej5_3 = TestCase (assertEqual "(head $ tail frecuenciaTokens) \",a,\"" (0.66666666)
                                   ((head $ tail frecuenciaTokens) ",a,"))
test_ej5_4 = TestCase (assertEqual "(head frecuenciaTokens) \"abc\"" (0)
                                   ((head frecuenciaTokens) "abc"))

tests_ej5 = TestList [TestLabel "test_ej5_1" test_ej5_1,TestLabel "test_ej5_2" test_ej5_2,TestLabel "test_ej5_3" test_ej5_3,TestLabel "test_ej5_4" test_ej5_4]


--TODO
--Que hacer si el texto es vacio?
frecuenciaTokens :: [Extractor]
frecuenciaTokens = map (\t -> (\x ->let longitud = length x in
                                     if longitud == 0 then
                                       0
                                     else
                                       ((fromIntegral $ repeticiones t x)/(fromIntegral $ longitud))
                               )
                       ) tokens

--Tests ej6
test_ej6_1 = TestCase (assertEqual "normalizarExtractor [\"lalala $$++$$ lalala lalala $$++$$\", \"  aa   x y aa \",\"aa aa\",\"aaa\"] repeticionesPromedio \"lalala $$++$$ lalala lalala $$++$$\""
                                   (1)
                                   (normalizarExtractor ["lalala $$++$$ lalala lalala $$++$$", "  aa   x y aa ","aa aa","aaa"] repeticionesPromedio "lalala $$++$$ lalala lalala $$++$$"))
                                   
test_ej6_2 = TestCase (assertEqual "normalizarExtractor [\"lalala $$++$$ lalala lalala $$++$$\", \"  aa   x y aa \",\"aa aa\",\"aaa\"] repeticionesPromedio \"  aa   x y aa \""
                                   (0.53333333333)
                                   (normalizarExtractor ["lalala $$++$$ lalala lalala $$++$$", "  aa   x y aa ","aa aa","aaa"] repeticionesPromedio "  aa   x y aa "))
                                   
test_ej6_3 = TestCase (assertEqual "normalizarExtractor [\"lalala $$++$$ lalala lalala $$++$$\", \"  aa   x y aa \",\"aa aa\",\"aaa\"] repeticionesPromedio \"aa aa\""
                                   (0.8)
                                   (normalizarExtractor ["lalala $$++$$ lalala lalala $$++$$", "  aa   x y aa ","aa aa","aaa"] repeticionesPromedio "aa aa"))
                                   
test_ej6_4 = TestCase (assertEqual "normalizarExtractor [\"lalala $$++$$ lalala lalala $$++$$\", \"  aa   x y aa \",\"aa aa\",\"aaa\"] repeticionesPromedio \"aaa\""
                                   (0.4)
                                   (normalizarExtractor ["lalala $$++$$ lalala lalala $$++$$", "  aa   x y aa ","aa aa","aaa"] repeticionesPromedio "aaa"))
                                   
test_ej6_5 = TestCase (assertEqual "normalizarExtractor [\"abc\"] (head frecuenciaTokens) \"abc\""
                                   (0)
                                   (normalizarExtractor ["abc"] (head frecuenciaTokens) "abc"))

tests_ej6 = TestList [TestLabel "test_ej6_1" test_ej6_1,TestLabel "test_ej6_2" test_ej6_2,TestLabel "test_ej6_3" test_ej6_3,TestLabel "test_ej6_4" test_ej6_4,
                      TestLabel "test_ej6_5" test_ej6_5]

normalizarExtractor :: [Texto] -> Extractor -> Extractor
normalizarExtractor textos extractor = let max = maximoValor textos extractor in
                                         if max == 0 then extractor
                                         else (/max).extractor

--Tests ej7
test_ej7_1 = TestCase (assertEqual "extraerFeatures [longitudPromedioPalabras, repeticionesPromedio] [\"b=a\", \"a = 2; a = 4\", \"C:/DOS C:/DOS/RUN RUN/DOS/RUN\"]"
                                   ([[0.33333334,0.6666667],[0.12962963,1.0],[1.0,0.6666667]])
                                   (extraerFeatures [longitudPromedioPalabras, repeticionesPromedio] ["b=a", "a = 2; a = 4", "C:/DOS C:/DOS/RUN RUN/DOS/RUN"]))

test_ej7_2 = TestCase (assertEqual "extraerFeatures [repeticionesPromedio, longitudPromedioPalabras] [\"a = 2; a = 4\", \"C:/DOS C:/DOS/RUN RUN/DOS/RUN\"]"
                                   ([[1.0,0.12962963],[0.6666667,1.0]])
                                   (extraerFeatures [repeticionesPromedio, longitudPromedioPalabras] ["a = 2; a = 4", "C:/DOS C:/DOS/RUN RUN/DOS/RUN"]))
test_ej7_3 = TestCase (assertEqual "extraerFeatures [longitudPromedioPalabras, repeticionesPromedio] [\"a = 2; a = 4\"]"
                                   ([[1.0, 1.0]])
                                   (extraerFeatures [longitudPromedioPalabras, repeticionesPromedio] ["a = 2; a = 4"]))
test_ej7_4 = TestCase (assertEqual "extraerFeatures [repeticionesPromedio] [\"C:/DOS C:/DOS/RUN RUN/DOS/RUN\", \"b=a\", \"a = 2; a = 4\"]"
                                   ([[0.6666667], [0.6666667],[1.0]])
                                   (extraerFeatures [repeticionesPromedio] ["C:/DOS C:/DOS/RUN RUN/DOS/RUN", "b=a", "a = 2; a = 4"]))

tests_ej7 = TestList [TestLabel "test_ej7_1" test_ej7_1,TestLabel "test_ej7_2" test_ej7_2,TestLabel "test_ej7_3" test_ej7_3,TestLabel "test_ej7_4" test_ej7_4]

--TODO
--Que hacer si la lista de extractores o de textos esta vacia?
extraerFeatures :: [Extractor] -> [Texto] -> Datos
--extraerFeatures extractores textos = foldr (\texto instancias -> (aplicarExtractores texto (normalizarExtractores textos extractores)):instancias) [] textos
--normaliza los extractores y luego por cada texto corre los extractores ya normalizados
extraerFeatures extractores textos = let extractoresNormalizadodos = normalizarExtractores textos extractores in
                                         map (\text -> map (\extractor -> extractor text) extractoresNormalizadodos
                                             ) textos

--Tests ej8-1
test_ej8_1_1 = TestCase (assertEqual "distEuclideana [1.0,0.75,0.8125] [0.75,1.0,0.5]"
                                   (0.47186464)
                                   (distEuclideana [1.0,0.75,0.8125] [0.75,1.0,0.5]))

test_ej8_1_2 = TestCase (assertEqual "distEuclideana [3.5] [8.3]"
                                   (4.8)
                                   (distEuclideana [3.5] [8.3]))
test_ej8_1_3 = TestCase (assertEqual "distEuclideana [0] [2]"
                                   (2)
                                   (distEuclideana [0] [2]))
test_ej8_1_4 = TestCase (assertEqual "distEuclideana [3.5] [0]"
                                   (3.5)
                                   (distEuclideana [3.5] [0]))
test_ej8_1_5 = TestCase (assertEqual "distEuclideana [0] [0]"
                                   (0)
                                   (distEuclideana [0] [0]))
test_ej8_1_6 = TestCase (assertEqual "distEuclideana [2.5,4] [2.5, 2]"
                                   (2)
                                   (distEuclideana [2.5,4] [2.5, 2]))
test_ej8_1_7 = TestCase (assertEqual "distEuclideana [2.5, 4, 3] [2.5, 2, 7.5]"
                                   (4.9244289)
                                   (distEuclideana [2.5, 4, 3] [2.5, 2, 7.5]))

tests_ej8_1 = TestList [TestLabel "test_ej8_1_1" test_ej8_1_1,TestLabel "test_ej8_1_2" test_ej8_1_2,TestLabel "test_ej8_1_3" test_ej8_1_3,TestLabel "test_ej8_1_4" test_ej8_1_4,
                      TestLabel "test_ej8_1_5" test_ej8_1_5, TestLabel "test_ej8_1_6" test_ej8_1_6, TestLabel "test_ej8_1_7" test_ej8_1_7]
                                             
distEuclideana :: Medida
distEuclideana = (\p q -> sqrt $ sum $ zipWith (\pv qv -> (pv-qv)*(pv-qv)) p q)

--Tests ej8-2
test_ej8_2_1 = TestCase (assertEqual "distCoseno [0,3,4] [0,-3,-4]"
                                   (-1.0)
                                   (distCoseno [0,3,4] [0,-3,-4]))

test_ej8_2_2 = TestCase (assertEqual "distCoseno [3.5] [8.3]"
                                   (1)
                                   (distCoseno [3.5] [8.3]))
test_ej8_2_3 = TestCase (assertEqual "distCoseno [2.4, 2] [2, 2.4]"
                                   (0.983606557)
                                   (distCoseno [2.4, 2] [2, 2.4]))
test_ej8_2_4 = TestCase (assertEqual "distCoseno [3.5, 2] [-1, 3]"
                                   (0.1961161351)
                                   (distCoseno [3.5, 2] [-1, 3]))

tests_ej8_2 = TestList [TestLabel "test_ej8_2_1" test_ej8_2_1,TestLabel "test_ej8_2_2" test_ej8_2_2,TestLabel "test_ej8_2_3" test_ej8_2_3,TestLabel "test_ej8_2_4" test_ej8_2_4]

tests_ej8 = TestList [TestLabel "tests_ej8_1" tests_ej8_1, TestLabel "tests_ej8_2" tests_ej8_2]

distCoseno :: Medida
distCoseno = (\p q -> (sumProductoEscalar p q) / ((sqrt (sumProductoEscalar p p))*(sqrt (sumProductoEscalar q q))) )

--Tests ej9
test_ej9_1 = TestCase (assertEqual "(knn 2 [[0,1],[0,2],[2,1],[1,1],[2,3]] [\"i\",\"i\",\"f\",\"f\",\"i\"] distEuclideana) [1,1]"
                                   ("f")
                                   ((knn 2 [[0,1],[0,2],[2,1],[1,1],[2,3]] ["i","i","f","f","i"] distEuclideana) [1,1]))
                                   
test_ej9_2 = TestCase (assertEqual "(knn 1 [[0,1],[0,2],[2,1],[0,0],[2,3]] [\"i\",\"i\",\"f\",\"f\",\"i\"] distEuclideana) [1,1]"
                                   ("f")
                                   ((knn 1 [[0,1],[0,2],[2,1],[0,0],[2,3]] ["i","i","f","f","i"] distEuclideana) [1,1]))
                                   
test_ej9_3 = TestCase (assertEqual "(knn 1 [[0,1],[0,2],[2,2],[3,2],[2,3]] [\"i\",\"i\",\"f\",\"f\",\"i\"] distEuclideana) [1,1]"
                                   ("i")
                                   ((knn 1 [[0,1],[0,2],[2,2],[3,2],[2,3]] ["i","i","f","f","i"] distEuclideana) [1,1]))
                                   
test_ej9_4 = TestCase (assertEqual "(knn 1 [[0,1]] [\"i\"] distEuclideana) [2.3, 1.2]"
                                   ("i")
                                   ((knn 1 [[0,1]] ["i"] distEuclideana) [2.3, 1.2]))
                                   
test_ej9_5 = TestCase (assertEqual "(knn 3 [[0,1]] [\"i\"] distEuclideana) [2.3, 1.2]"
                                   ("i")
                                   ((knn 3 [[0,1]] ["i"] distEuclideana) [2.3, 1.2]))
                                   
test_ej9_6 = TestCase (assertEqual "(knn 3 [[1,0.2,0.4],[0.8, 0.21, 0],[0.2, 1.01, 2.3],[1.2, 0.1, 0.2],[0.3, 1.2, 0.001],[0.2, 0.023, 0.25]] [\"f\",\"f\",\"f\",\"i\",\"i\",\"i\"] distCoseno) [0.3, 1, 0.25]"
                                   ("i")
                                   ((knn 3 [[1,0.2,0.4],[0.8, 0.21, 0],[0.2, 1.01, 2.3],[1.2, 0.1, 0.2],[0.3, 1.2, 0.001],[0.2, 0.023, 0.25]] ["f","f","f","i","i","i"] distCoseno) [0.3, 1, 0.25]))

tests_ej9 = TestList [TestLabel "test_ej9_1" test_ej9_1,TestLabel "test_ej9_2" test_ej9_2,TestLabel "test_ej9_3" test_ej9_3,TestLabel "test_ej9_4" test_ej9_4,
                      TestLabel "test_ej9_5" test_ej9_5, TestLabel "test_ej9_6" test_ej9_6]
--TODO
--Se puede asumir que la longitud de lista de instancias (datos) es igual a la de las etiquetas
--Está bien que para la parte de la moda estadistica contamos cuantas veces se repite cada elemento y nos quedamos con el que más se repite?
--Que pasa si estas listas son vacias?
--Se puede asumir que k es mayor a 0
knn :: Int -> Datos -> [Etiqueta] -> Medida -> Modelo
knn k datos etiquetas distancia = (\valor -> snd (mejor (cuentas (kMenores k datos etiquetas distancia valor))))

--Tests ej10
test_ej10_1 = TestCase (assertEqual "separarDatos [[1,1],[2,2],[3,3],[4,4],[5,5],[6,6],[7,7]] [\"1\",\"2\",\"3\",\"4\",\"5\",\"6\",\"7\"] 3 2"
                                   (([[1.0,1.0],[2.0,2.0],[5.0,5.0],[6.0,6.0]],[[3.0,3.0],[4.0,4.0]], ["1","2","5","6"], ["3","4"]))
                                   (separarDatos [[1,1],[2,2],[3,3],[4,4],[5,5],[6,6],[7,7]] ["1","2","3","4","5","6","7"] 3 2))
                                   
test_ej10_2 = TestCase (assertEqual "separarDatos [[1,1],[2,2],[3,3],[4,4],[5,5],[6,6]] [\"1\",\"2\",\"3\",\"4\",\"5\",\"6\"] 3 3"
                                   (([[1.0,1.0],[2.0,2.0],[3.0,3.0],[4.0,4.0]],[[5.0,5.0],[6.0,6.0]], ["1","2","3","4"], ["5","6"]))
                                   (separarDatos [[1,1],[2,2],[3,3],[4,4],[5,5],[6,6]] ["1","2","3","4","5","6"] 3 3))
                                   
test_ej10_3 = TestCase (assertEqual "separarDatos [[1,1]] [\"1\"] 1 1"
                                   (([],[[1.0,1.0]], [], ["1"]))
                                   (separarDatos [[1,1]] ["1"] 1 1))
                                   
test_ej10_4 = TestCase (assertEqual "separarDatos [[1,1],[2,2],[3,3],[4,4],[5,5],[6,6],[7,7]] [\"1\",\"2\",\"3\",\"4\",\"5\",\"6\",\"7\"] 1 1"
                                   (([],[[1.0,1.0],[2.0,2.0],[3.0,3.0],[4.0,4.0],[5.0,5.0],[6.0,6.0],[7.0,7.0]], [], ["1","2","3","4","5","6", "7"]))
                                   (separarDatos [[1,1],[2,2],[3,3],[4,4],[5,5],[6,6],[7,7]] ["1","2","3","4","5","6","7"] 1 1))
                                   
test_ej10_5 = TestCase (assertEqual "separarDatos [[1,1],[2,2],[3,3],[4,4],[5,5],[6,6],[7,7]] [\"1\",\"2\",\"3\",\"4\",\"5\",\"6\",\"7\"] 7 3"
                                   (([[1.0,1.0],[2.0,2.0],[4.0,4.0],[5.0,5.0],[6.0,6.0],[7.0,7.0]],[[3.0,3.0]], ["1","2","4","5","6","7"], ["3"]))
                                   (separarDatos [[1,1],[2,2],[3,3],[4,4],[5,5],[6,6],[7,7]] ["1","2","3","4","5","6","7"] 7 3))

tests_ej10 = TestList [TestLabel "test_ej10_1" test_ej10_1,TestLabel "test_ej10_2" test_ej10_2,TestLabel "test_ej10_3" test_ej10_3,TestLabel "test_ej10_4" test_ej10_4,
                      TestLabel "test_ej10_5" test_ej10_5]

--TODO
--Podemos asumir que el n no es mayor a la cantidad de elementos?
separarDatos :: Datos -> [Etiqueta] -> Int -> Int -> (Datos, Datos, [Etiqueta], [Etiqueta])
--separarDatos datos etiquetas n p = (fst(unzip(dameTrain (take ((calcularTamano datos n) * n) (zip datos etiquetas)) p (calcularTamano datos n))),
--                        fst (unzip(dameVal (take ((calcularTamano datos n) * n) (zip datos etiquetas)) p (calcularTamano datos n))), 
--                        snd(unzip(dameTrain (take ((calcularTamano datos n) * n) (zip datos etiquetas)) p (calcularTamano datos n))),
--                        snd (unzip(dameVal (take ((calcularTamano datos n) * n) (zip datos etiquetas))  p (calcularTamano datos n))))
separarDatos datos etiquetas n p = let tamanoParticion = (length datos) `quot` n in
                                     let particion = take (tamanoParticion*n) (zip datos etiquetas) in
                                       let primerosElementos = tamanoParticion * (p-1) in
                                         let train = (take primerosElementos particion) ++ (drop (p*tamanoParticion) particion) in
                                           let val = take tamanoParticion (drop primerosElementos particion) in
                                             (fst (unzip train),
                                              fst (unzip val),
                                              snd (unzip train),
                                              snd (unzip val))

--Tests ej11
test_ej11_1 = TestCase (assertEqual "accuracy [\"f\", \"f\", \"i\", \"i\", \"f\"] [\"i\", \"f\", \"i\", \"f\", \"f\"]"
                                   (0.6)
                                   (accuracy ["f", "f", "i", "i", "f"] ["i", "f", "i", "f", "f"]))
                                   
test_ej11_2 = TestCase (assertEqual "accuracy [\"f\", \"f\", \"f\", \"f\", \"f\"] [\"f\", \"f\", \"f\", \"f\", \"f\"]"
                                   (1)
                                   (accuracy ["f", "f", "f", "f", "f"] ["f", "f", "f", "f", "f"]))
                                   
test_ej11_3 = TestCase (assertEqual "accuracy [\"f\", \"f\", \"f\", \"f\", \"f\"] [\"i\", \"i\", \"i\", \"i\", \"i\"]"
                                   (0)
                                   (accuracy ["f", "f", "f", "f", "f"] ["i", "i", "i", "i", "i"]))
                                   
test_ej11_4 = TestCase (assertEqual "accuracy [\"f\"] [\"i\"]"
                                   (0)
                                   (accuracy ["f"] ["i"]))
                                   
test_ej11_5 = TestCase (assertEqual "accuracy [\"f\"] [\"f\"]"
                                   (1)
                                   (accuracy ["f"] ["f"]))

test_ej11_6 = TestCase (assertEqual "accuracy [\"i\",\"i\",\"i\",\"i\"] [\"f\",\"i\",\"f\",\"f\"]"
                                   (0.25)
                                   (accuracy ["i","i","i","i"] ["f","i","f","f"]))

tests_ej11 = TestList [TestLabel "test_ej11_1" test_ej11_1,TestLabel "test_ej11_2" test_ej11_2,TestLabel "test_ej11_3" test_ej11_3,TestLabel "test_ej11_4" test_ej11_4,
                      TestLabel "test_ej11_5" test_ej11_5,TestLabel "test_ej11_6" test_ej11_6]

--TODO
--Que hacer si me pasan listas vacias?
accuracy :: [Etiqueta] -> [Etiqueta] -> Float
accuracy xs ys = realToFrac (contarIguales $ zip xs ys) / (genericLength xs)
                  where contarIguales = foldr(\x acum -> if(fst(x) == snd(x)) then acum+1 else acum) 0 


--accuracies::Datos -> [Etiqueta]->Int -> [Feature]
--accuracies datos etiquetas n = map(\particionDeValidacion -> accuracy (cuarto $ separarDatos datos etiquetas n particionDeValidacion) (etiquetasObtenidas datos etiquetas n particionDeValidacion (segundo $ separarDatos datos etiquetas n particionDeValidacion))) [1..n]

--Tests ej12
test_ej12_1 = TestCase (assertEqual "nFoldCrossValidation 4 [[1,1],[2,2],[3,3],[4,4]] [\"i\",\"f\",\"f\",\"i\"]"
                                   (0.25)
                                   (nFoldCrossValidation 4 [[1,1],[2,2],[3,3],[4,4]] ["i","f","f","i"]))
                                   
test_ej12_2 = TestCase (assertEqual "nFoldCrossValidation 2 [[1,1],[2,2],[3,3],[4,4]] [\"f\",\"f\",\"f\",\"i\"]"
                                   (0.75)
                                   (nFoldCrossValidation 2 [[1,1],[2,2],[3,3],[4,4]] ["f","f","f","i"]))
                                   
--test_ej12_3 = TestCase (assertEqual "nFoldCrossValidation 1 [[1,1],[2,2],[3,3],[4,4]] [\"f\",\"f\",\"f\",\"i\"]"
--                                   (0.75)
--                                   (nFoldCrossValidation 1 [[1,1],[2,2],[3,3],[4,4]] ["f","f","f","i"]))
                                   
test_ej12_4 = TestCase (assertEqual "nFoldCrossValidation 2 [[1,1],[2,2],[3,3],[4,4],[5,5]] [\"f\",\"f\",\"f\",\"i\",\"f\"]"
                                   (0.75)
                                   (nFoldCrossValidation 2 [[1,1],[2,2],[3,3],[4,4],[5,5]] ["f","f","f","i","f"]))

tests_ej12 = TestList [TestLabel "test_ej12_1" test_ej12_1,TestLabel "test_ej12_2" test_ej12_2,
                      --TestLabel "test_ej12_3" test_ej12_3,
                      TestLabel "test_ej12_4" test_ej12_4]
--TODO
--Si la división de los datos por n lo es exacta se descartan los casos sobrantes (como en el ej10).
--Pero al hacer esto si tengo 4 elementos y los divido en 3 particiones se descarta el cuarto, por lo cual nunca se usa esa particion como dato de entrenamiento
--y tampoco como dato de validacion. Está bien eso?

--Si las particiones tienen más de un elemento, por ejemplo si los elementos son [[1],[2],[3],[4]] y cada particion tiene 2 elementos nosotros los que hacemos
--es calcular el (knn 15 [[3],[4]] etiquetas de 3 y 4 distEuclideana) [1], luego (knn 15 [[3],[4]] etiquetas de 3 y 4 distEuclideana) [2] y calculamos el
--accuracy de estas dos etiquetas obtenidas con las 2 originales de [1] y [2]. Luego los mismo pero usando [[1],[2]] como entrenamiento y [3] y luego [4] como validacion.
--Por ultimo calculamos el accuracy entre estas dos accuracies obtenidas
--Esta bien esto?

--Que pasa si n = 1, ya que estaria llamando a knn con listas vacias?
--Que pasa si se pasa una lista con un elemento, ya que las de entrenamiento o la de validacion quedaría una lista vacia?
nFoldCrossValidation :: Int -> Datos -> [Etiqueta] -> Float
nFoldCrossValidation n datos etiquetas = mean accuracies
    where accuracyPorParticion particionDeValidacion = let (x_train, x_val, y_train, y_val) = separarDatos datos etiquetas n particionDeValidacion in
                                                        let modeloK15 = (knn 15 (x_train) (y_train) distEuclideana) in
                                                          --let etiquetasObtenidas = foldr(\instancia acum -> (modeloK15 instancia):acum) [] in
                                                          let etiquetasObtenidas = map modeloK15 in
                                                            accuracy (y_val) (etiquetasObtenidas (x_val))
          accuracies = [accuracyPorParticion x | x<-[1..n]]

tests = TestList [TestLabel "tests_ej1" tests_ej1, TestLabel "tests_ej2" tests_ej2, TestLabel "tests_ej3" tests_ej3, TestLabel "tests_ej4" tests_ej4,
                  TestLabel "tests_ej5" tests_ej5, TestLabel "tests_ej6" tests_ej6, TestLabel "tests_ej7" tests_ej7, TestLabel "tests_ej8" tests_ej8,
                  TestLabel "tests_ej9" tests_ej9, TestLabel "tests_ej10" tests_ej10, TestLabel "tests_ej11" tests_ej11, TestLabel "tests_ej12" tests_ej12]
          
--etiquetasObtenidas::Datos -> [Etiqueta]->Int->Int->[Instancia]->[Etiqueta]
--etiquetasObtenidas datos etiquetas n particionDeValidacion instancias = foldr(\instancia acum -> (modeloK15 datos etiquetas n particionDeValidacion instancia):acum) [] instancias

--modeloK15::Datos -> [Etiqueta]->Int->Int->Modelo
--modeloK15 datos etiquetas n particionDeValidacion = (knn 15 (primero $ separarDatos datos etiquetas n particionDeValidacion) (tercero $ separarDatos datos etiquetas n particionDeValidacion) distEuclideana)

-- *************** Funciones auxiliares ********************

--cantidadDeApariciones::Eq a => [a]->([a]->[(Int,a)])
--cantidadDeApariciones = foldr (\x contar -> (\ys->((repeticiones x ys),x):contar ys)) (\ys->[])

repeticiones::Eq a =>a->[a]->Int
repeticiones x = foldr (\y contar -> if x==y then 1 + contar else contar) 0

--elementosSinRepetir :: Eq a => [a] -> [a]
--elementosSinRepetir xs = (foldr(\x recu->(\ys -> if repeticiones x (tail ys) == 0 then x:recu (tail ys) else recu (tail ys))) (\ys->[]) xs) xs
elementosSinRepetir :: Eq a => [a] -> [a]
elementosSinRepetir = foldr (\x recu -> if x `elem` recu then recu else x:recu) []

--esta :: Eq a => a -> [a] -> Bool
--esta x xs = foldr (\y  recu -> y == x || recu) False xs

--apariciones :: Eq a => a -> [a] -> Int
--apariciones a = foldr (\x xs -> if x==a then (1+xs) else xs) 0

maximoValor :: [Texto] -> Extractor -> Feature
maximoValor textos extractor = abs $ maximoAbsoluto $ ejecutarExtractor textos extractor

maximoAbsoluto :: [Feature] -> Feature
--maximoAbsoluto = foldr (\x buscarMax -> if abs x >= abs buscarMax then abs x else abs buscarMax) 0
maximoAbsoluto = maximum.(map abs)

ejecutarExtractor:: [Texto] -> Extractor -> [Feature]
ejecutarExtractor textos extractor = map extractor textos

--aplicarExtractores :: Texto -> [Extractor] -> Instancia
--aplicarExtractores texto extractores = map (\extractor -> extractor texto) extractores

normalizarExtractores :: [Texto] -> [Extractor] -> [Extractor]
normalizarExtractores textos extractores = map (\ext -> normalizarExtractor textos ext) extractores 

-- Para ejercicio 8

sumProductoEscalar :: Instancia -> Instancia -> Feature
sumProductoEscalar p q = sum (zipWith (*) p q)


-- Para ejercicio 9

kMenores :: Int -> Datos -> [Etiqueta] -> Medida -> Instancia -> [(Instancia,Etiqueta)]
kMenores k datos etiquetas distancia valor = take k $ sortBy 
    (\a b -> (if ((distancia (fst a) valor) < (distancia (fst b) valor)) then LT else GT )) (zip datos etiquetas)

mejor :: [(Int,a)] -> a
mejor xs = snd (maximumBy (\a b -> if (fst a)<(fst b) then LT else GT) xs)

-- Para ejercicio 10

--contarIguales ::Eq a =>  [(a,a)] -> Int
--contarIguales = foldr(\x acum -> if(fst(x) == snd(x)) then acum+1 else acum) 0 

-- Para ejercicio 11

--dameTrain:: [(Instancia,Etiqueta)] -> Int -> Int -> [(Instancia,Etiqueta)]
--dameTrain xs p tamano = (take (tamano * (p-1)) xs) ++ (drop (p*tamano) xs )

--dameVal:: [(Instancia,Etiqueta)] -> Int -> Int -> [(Instancia,Etiqueta)]
--dameVal xs p tamano = take tamano (drop ((p-1)*tamano) xs)

--calcularTamano:: [a] -> Int -> Int
--calcularTamano xs n = (length xs) `quot` n

-- Para ejercicio 12


primero :: (a,b,c,d) -> a
primero (x,y,z,w) = x

segundo :: (a,b,c,d) -> b
segundo (x,y,z,w) = y

tercero :: (a,b,c,d) -> c
tercero (x,y,z,w) = z

cuarto :: (a,b,c,d) -> d
cuarto (x,y,z,w) = w