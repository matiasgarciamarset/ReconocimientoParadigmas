module Solucion where

import Data.List

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

--ej1
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
--ej2
longitudPromedioPalabras :: Extractor
longitudPromedioPalabras xs = mean $ map genericLength palabras
                               where palabras = split ' ' xs

--ej3
cuentas :: Eq a => [a] -> [(Int, a)]
cuentas xs = let sinRepetidos = elementosSinRepetir $ reverse xs in
               map (\x -> ((repeticiones x xs), x)) sinRepetidos

--ej4
repeticionesPromedio :: Extractor
repeticionesPromedio xs = let palabras = split ' ' xs in
                            (fromIntegral $ length palabras) / (fromIntegral $ length $ elementosSinRepetir palabras)

tokens :: [Char]
tokens = "_,)(*;-=>/.{}\"&:+#[]<|%!\'@?~^$` abcdefghijklmnopqrstuvwxyz0123456789"

--ej5
frecuenciaTokens :: [Extractor]
frecuenciaTokens = map (\t -> (\x ->let longitud = length x in
                                     if longitud == 0 then
                                       0
                                     else
                                       ((fromIntegral $ repeticiones t x)/(fromIntegral $ longitud))
                               )
                       ) tokens

--ej6
normalizarExtractor :: [Texto] -> Extractor -> Extractor
normalizarExtractor textos extractor = let max = maximoValor textos extractor in
                                         if max == 0 then extractor
                                         else (/max).extractor

--ej7
extraerFeatures :: [Extractor] -> [Texto] -> Datos
extraerFeatures extractores textos = let extractoresNormalizadodos = normalizarExtractores textos extractores in
                                         map (\text -> map (\extractor -> extractor text) extractoresNormalizadodos
                                             ) textos

--ej8-1                                             
distEuclideana :: Medida
distEuclideana = (\p q -> sqrt $ sum $ zipWith (\pv qv -> (pv-qv)*(pv-qv)) p q)

--ej8-2
distCoseno :: Medida
distCoseno = (\p q -> (sumProductoEscalar p q) / ((sqrt (sumProductoEscalar p p))*(sqrt (sumProductoEscalar q q))) )

--ej9
knn :: Int -> Datos -> [Etiqueta] -> Medida -> Modelo
knn k datos etiquetas distancia = (\valor -> mejor $ cuentas $ map (\(x, y)->y) (kMenores k datos etiquetas distancia valor))

--ej10
separarDatos :: Datos -> [Etiqueta] -> Int -> Int -> (Datos, Datos, [Etiqueta], [Etiqueta])
separarDatos datos etiquetas n p = let tamanoParticion = (length datos) `quot` n in
                                     let particion = take (tamanoParticion*n) (zip datos etiquetas) in
                                       let primerosElementos = tamanoParticion * (p-1) in
                                         let train = (take primerosElementos particion) ++ (drop (p*tamanoParticion) particion) in
                                           let val = take tamanoParticion (drop primerosElementos particion) in
                                             (fst (unzip train),
                                              fst (unzip val),
                                              snd (unzip train),
                                              snd (unzip val))

--ej11
accuracy :: [Etiqueta] -> [Etiqueta] -> Float
accuracy xs ys = realToFrac (contarIguales $ zip xs ys) / (genericLength xs)
                  where contarIguales = foldr(\x acum -> if(fst(x) == snd(x)) then acum+1 else acum) 0 

--ej12
nFoldCrossValidation :: Int -> Datos -> [Etiqueta] -> Float
nFoldCrossValidation n datos etiquetas = mean accuracies
    where accuracyPorParticion particionDeValidacion = let (x_train, x_val, y_train, y_val) = separarDatos datos etiquetas n particionDeValidacion in
                                                        let modeloK15 = (knn 15 (x_train) (y_train) distEuclideana) in
                                                          let etiquetasObtenidas = map modeloK15 in
                                                            accuracy (y_val) (etiquetasObtenidas (x_val))
          accuracies = [accuracyPorParticion x | x<-[1..n]]

-- *************** Funciones auxiliares ********************

repeticiones::Eq a =>a->[a]->Int
repeticiones x = foldr (\y contar -> if x==y then 1 + contar else contar) 0

elementosSinRepetir :: Eq a => [a] -> [a]
elementosSinRepetir = foldr (\x recu -> if x `elem` recu then recu else recu++[x]) []

maximoValor :: [Texto] -> Extractor -> Feature
maximoValor textos extractor = abs $ maximoAbsoluto $ ejecutarExtractor textos extractor

maximoAbsoluto :: [Feature] -> Feature
maximoAbsoluto = maximum.(map abs)

ejecutarExtractor:: [Texto] -> Extractor -> [Feature]
ejecutarExtractor textos extractor = map extractor textos

normalizarExtractores :: [Texto] -> [Extractor] -> [Extractor]
normalizarExtractores textos extractores = map (\ext -> normalizarExtractor textos ext) extractores 

sumProductoEscalar :: Instancia -> Instancia -> Feature
sumProductoEscalar p q = sum (zipWith (*) p q)

kMenores :: Int -> Datos -> [Etiqueta] -> Medida -> Instancia -> [(Instancia,Etiqueta)]
kMenores k datos etiquetas distancia valor = take k $ sortBy 
    (\a b -> (if ((distancia (fst a) valor) < (distancia (fst b) valor)) then LT else GT )) (zip datos etiquetas)

mejor :: [(Int,a)] -> a
mejor xs = snd (maximumBy (\a b -> if (fst a)<(fst b) then LT else GT) xs)