module Tests where

import Test.HUnit
import Solucion

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

--Tests ej2
test_ej2_1 = TestCase (assertEqual "longitudPromedioPalabras \"Este test tiene palabras $$++$$\"" (5.4)
                                   (longitudPromedioPalabras "Este test tiene palabras $$++$$"))
test_ej2_2 = TestCase (assertEqual "longitudPromedioPalabras \"  12345    12  \"" (3.5)
                                   (longitudPromedioPalabras "  12345    12  "))
test_ej2_3 = TestCase (assertEqual "longitudPromedioPalabras \"Este\"" (4)
                                   (longitudPromedioPalabras "Este"))

tests_ej2 = TestList [TestLabel "test_ej2_1" test_ej2_1,TestLabel "test_ej2_2" test_ej2_2,TestLabel "test_ej2_3" test_ej2_3]

--Tests ej3
test_ej3_1 = TestCase (assertEqual "cuentas [\"x\", \"x\", \"y\", \"x\", \"z\"]" [(3,"x"),(1,"y"),(1,"z")]
                                   (cuentas ["x", "x", "y", "x", "z"]))
test_ej3_2 = TestCase (assertEqual "cuentas []" ([]::[(Int, String)])
                                   (cuentas []))
test_ej3_3 = TestCase (assertEqual "cuentas [\"x\"]" [(1,"x")]
                                   (cuentas ["x"]))
test_ej3_4 = TestCase (assertEqual "cuentas [\"x\", \"x\", \"yy\", \"y\", \"z\", \"x\"]" [(3,"x"), (1,"yy"),(1,"y"),(1,"z")]
                                   (cuentas ["x", "x", "yy", "y", "z", "x"]))
test_ej3_5 = TestCase (assertEqual "cuentas [[\"x\"], [\"x\"], [\"yy\"], [\"y\"], [\"y\",\"y\"], [\"z\"], [\"y\",\"y\"], [\"x\"]]" [(3,["x"]), (1,["yy"]),(1,["y"]), (2,["y","y"]),(1,["z"])]
                                   (cuentas [["x"], ["x"], ["yy"], ["y"], ["y","y"], ["z"], ["y","y"], ["x"]]))

tests_ej3 = TestList [TestLabel "test_ej3_1" test_ej3_1,TestLabel "test_ej3_2" test_ej3_2,TestLabel "test_ej3_3" test_ej3_3,TestLabel "test_ej3_4" test_ej3_4,
                      TestLabel "test_ej3_5" test_ej3_5]

--Tests ej4
test_ej4_1 = TestCase (assertEqual "repeticionesPromedio \"lalala $$++$$ lalala lalala $$++$$\"]" (2.5)
                                   (repeticionesPromedio "lalala $$++$$ lalala lalala $$++$$"))
test_ej4_2 = TestCase (assertEqual "repeticionesPromedio \"  aa   x y aa \"]" (1.3333333333)
                                   (repeticionesPromedio "  aa   x y aa "))
test_ej4_3 = TestCase (assertEqual "repeticionesPromedio \"aa aa\"]" (2)
                                   (repeticionesPromedio "aa aa"))
test_ej4_4 = TestCase (assertEqual "repeticionesPromedio \"aaa\"]" (1)
                                   (repeticionesPromedio "aaa"))

tests_ej4 = TestList [TestLabel "test_ej4_1" test_ej4_1,TestLabel "test_ej4_2" test_ej4_2,TestLabel "test_ej4_3" test_ej4_3,TestLabel "test_ej4_4" test_ej4_4]

--Tests ej5
test_ej5_1 = TestCase (assertEqual "(head frecuenciaTokens) \"use_snake_case !\"" (0.125)
                                   ((head frecuenciaTokens) "use_snake_case !"))
test_ej5_2 = TestCase (assertEqual "(head frecuenciaTokens) \"\"" (0)
                                   ((head frecuenciaTokens) ""))
test_ej5_3 = TestCase (assertEqual "(head $ tail frecuenciaTokens) \",a,\"" (0.66666666)
                                   ((head $ tail frecuenciaTokens) ",a,"))
test_ej5_4 = TestCase (assertEqual "(head frecuenciaTokens) \"abc\"" (0)
                                   ((head frecuenciaTokens) "abc"))

tests_ej5 = TestList [TestLabel "test_ej5_1" test_ej5_1,TestLabel "test_ej5_2" test_ej5_2,TestLabel "test_ej5_3" test_ej5_3,TestLabel "test_ej5_4" test_ej5_4]

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

--Tests ej12
test_ej12_1 = TestCase (assertEqual "nFoldCrossValidation 4 [[1,1],[2,2],[3,3],[4,4]] [\"i\",\"f\",\"f\",\"i\"]"
                                   (0.0)
                                   (nFoldCrossValidation 4 [[1,1],[2,2],[3,3],[4,4]] ["i","f","f","i"]))
                                   
test_ej12_2 = TestCase (assertEqual "nFoldCrossValidation 2 [[1,1],[2,2],[3,3],[4,4]] [\"f\",\"f\",\"f\",\"i\"]"
                                   (0.75)
                                   (nFoldCrossValidation 2 [[1,1],[2,2],[3,3],[4,4]] ["f","f","f","i"]))
                                   
test_ej12_3 = TestCase (assertEqual "nFoldCrossValidation 2 [[1,1],[2,2],[3,3],[4,4],[5,5]] [\"f\",\"f\",\"f\",\"i\",\"f\"]"
                                   (0.75)
                                   (nFoldCrossValidation 2 [[1,1],[2,2],[3,3],[4,4],[5,5]] ["f","f","f","i","f"]))

tests_ej12 = TestList [TestLabel "test_ej12_1" test_ej12_1,TestLabel "test_ej12_2" test_ej12_2, TestLabel "test_ej12_3" test_ej12_3]

--tests
tests = TestList [TestLabel "tests_ej1" tests_ej1, TestLabel "tests_ej2" tests_ej2, TestLabel "tests_ej3" tests_ej3, TestLabel "tests_ej4" tests_ej4,
                  TestLabel "tests_ej5" tests_ej5, TestLabel "tests_ej6" tests_ej6, TestLabel "tests_ej7" tests_ej7, TestLabel "tests_ej8" tests_ej8,
                  TestLabel "tests_ej9" tests_ej9, TestLabel "tests_ej10" tests_ej10, TestLabel "tests_ej11" tests_ej11, TestLabel "tests_ej12" tests_ej12]