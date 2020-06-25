module TpHaskell2Nacho where

import LoadFile
import TpHaskell2Estructuras
import TpHaskell2Funciones
import TpHaskell2LeoArchivo
import Data.List

{-
-
- INICIO DE FUNCIONES
-
-}

-- Esto lo uso para probar

archivoPrueba = Archivo (NombreM "ArchivoPrueba") [importPruebaImports,importPruebaImports2] [dataPruebaData,dataPruebaData2] [clasePruebaClases,clasePruebaClases2] [] []

dataPruebaData2 = Data "Prueba" "= Normal | CML | CIL | Comillas" (Just "prueba de Data con comentario")
clasePruebaClases2 = Clase (Just "(Monad m, Monad (t m))") "Transform" "t m" [] (Just "prueba de Calse con comentario")

{- Todo esto está en Funciones

archivoPruebaModulo = Archivo (NombreM "TpHaskell2Index") [] [] [] [] []
modulo :: Archivo -> Quizas NombreM -- Devuelve el módulo
modulo (Archivo nm _ _ _ _ _) = OK nm
-- para probar en GHCI : modulo $ archivoPruebaModulo

importPruebaImports = Importacion (NombreM "Data.List (inits)") (Just "prueba") --"-- agrego lista"
importPruebaImports2 = Importacion (NombreM "Data.List") Nothing
archivoPruebaImports = Archivo (NombreM "") [importPruebaImports, importPruebaImports2] [] [] [] []
imports :: Archivo -> [Importacion] -- Módulos que importa
imports (Archivo _ im _ _ _ _) = im
-- para probar en GHCI : imports $ archivoPruebaImports

dataPruebaData  = Data "Estoy" "= Normal | CML | CIL | Comillas" Nothing
dataPruebaData2 = Data "Prueba" "= Normal | CML | CIL | Comillas" (Just "prueba de Data con comentario")
archivoPruebaDatas = Archivo (NombreM "") [] [dataPruebaData] [] [] []
datas :: Archivo -> [Data] -- Tipos de dato que genera
datas (Archivo _ _ d _ _ _) = d
-- para probar en GHCI : datas $ archivoPruebaDatas

clasePruebaClases  = Clase (Just "(Monad m, Monad (t m))") "Transform" "t m" [] Nothing
clasePruebaClases2 = Clase (Just "(Monad m, Monad (t m))") "Transform" "t m" [] (Just "prueba de Calse con comentario")
archivoPruebaClases = Archivo (NombreM "") [] [] [clasePruebaClases] [] []
clases :: Archivo -> [Clase] -- Clases que genera
clases (Archivo _ _ _ c _ _) = c
-- para probar en GHCI : clases $ archivoPruebaClases

-- TODO: completar estas que quedaron
{-
funciones :: Archivo -> [Funciones] -- Funciones definidas
instancias :: NombreM -> Archivo -> Quizas [Instancias] -- Devuelve las instancias definidas de un tipo de dato
nombres :: Archivo -> [Nombre] -- Lista de clases, datos, funciones, etc incluidas (solo de primer nivel).
-}

bloqueFuncionPrueba1 = Expresion "a | a < 5 = a + 1"
bloqueFuncionPrueba2 = Expresion "a = a"
patronFuncionPrueba = Patron (Left bloqueFuncionPrueba1) [] Nothing
patronFuncionPrueba2 = Patron (Left bloqueFuncionPrueba2 ) [] (Just "-- comento esto")
funcionPruebaAgregoFuncion = Funcion (NombreF "miFuncion") (Just "a -> a") [patronFuncionPrueba, patronFuncionPrueba2] Nothing
archivoConFuncionAgregada = fromQuizas $ agregoFuncion funcionPruebaAgregoFuncion archivoPruebaModulo

agregoFuncion :: Funcion -> Archivo -> Quizas Archivo -- Agrega una función
-- tiene un Quizas archivo porque si ya existe la funcion devuelvo un error
agregoFuncion f (Archivo nm im d c ins fa) | yaExisteFuncion f fa = Error "La funcion ya existe"
agregoFuncion f (Archivo nm im d c ins fa) = OK (Archivo nm im d c ins (f:fa))

yaExisteFuncion :: Funcion -> [Funcion] -> Bool
yaExisteFuncion f (fa:farest) = if funcionesIguales f fa then True else yaExisteFuncion f farest
yaExisteFuncion _ _ = False

funcionesIguales :: Funcion -> Funcion -> Bool
funcionesIguales (Funcion nomf1 _ _ _) (Funcion nomf2 _ _ _) = (nomf1 == nomf2)

sacoFuncion :: NombreF -> Archivo -> Archivo -- Devuelve el Archivo sacando una función
sacoFuncion f (Archivo nm im d c ins fa) = Archivo nm im d c ins (sacoFuncion' (creoFuncionSoloConNombre f) fa)

sacoFuncion' _ []                 = []
sacoFuncion' x@(Funcion f1 _ _ _) (y@(Funcion f2 _ _ _):ys) | f1 == f2    = sacoFuncion' x ys
                                                            | otherwise   = y : sacoFuncion' x ys

creoFuncionSoloConNombre :: NombreF -> Funcion
creoFuncionSoloConNombre f = Funcion f Nothing [] Nothing

-- Esto no es necesario sacoFuncion f (Archivo nm im d c ins fa) = Error "La funcion no existe"

--sacoFuncion :: NombreF -> Archivo -> Archivo -- Devuelve el Archivo sacando una función
-}




-- Ignacio Segura:

-- data Opciones = Opciones {largoLinea :: Int, anchoTab :: Int, comentarioInline :: Bool, compacto :: Bool, colores :: Bool} -- Pueden agregar otros parámetros.

{-
largoLinea: corta las líneas cada x caracteres, pero no podemos cortar en medio de una palabra. 
    Una barra "\" indica la continuación de la línea anterior.
anchoTab: determina el ancho de los tabs. 1 espacio, 2 espacios, 3 espacios...
comentarioInline: 
compacto: Borraría los saltos de línea intermedios, siempre y cuando todo siga funcionando. Quedaría visualmente feo.
colores: activa colores para lo que definamos. Es más avanzado.
-}

-- defaultOpciones :: Opciones


instance Show Archivo where
    show = showArchivo

-- showArchivo :: Opciones -> Archivo -> String -- Muestra el archivo con formato
showArchivo :: Archivo -> String 
showArchivo a@(Archivo {nombreArc = nA})    = "module " ++ showNombre (NM nA) ++ " where" ++ "\n" ++ showArchivo' a 

showArchivo' :: Archivo -> String 
showArchivo' a@(Archivo {importsArc = (imp:imps)})                                                   = showImportacion imp ++ "\n" ++ showArchivo' (a {importsArc = imps})
showArchivo' a@(Archivo {importsArc = [], clasesArc = (c:cs)})                                       = showClase c ++ "\n" ++ showArchivo' (a {clasesArc = cs})
showArchivo' a@(Archivo {importsArc = [], clasesArc = [], instancesArc = (inst:insts)})              = showInstancia inst ++ "\n" ++ showArchivo' (a {instancesArc = insts})
showArchivo' a@(Archivo {importsArc = [], clasesArc = [], instancesArc = [], funcionesArc = (f:fs)}) = showFuncion f ++ "\n" ++ showArchivo' (a {funcionesArc = fs})

{- 
showArchivo a@(Archivo {nombreArc = nA, importsArc = impArcs, datasArc = datasArcs})    = "module " ++ showNombre (NM nA) ++ " where" ++ "\n" ++ showArchivoImp(impArcs)

showArchivoImp :: [Importacion] -> String
showArchivoImp []            = ""
showArchivoImp (imp:imps)    = showImportacion imp ++ "\n" ++ showArchivoImp imps

showArchivoData :: [Data] -> String
showArchivoData a@(Archivo {datasArc = []})      = ""
showArchivoData a@(Archivo {datasArc = (d:ds)})  = showData d ++ "\n" ++ showArchivoData (a {datasArc = ds})
-}

instance Show Importacion where
    show = showImportacion

showImportacion :: Importacion -> String
showImportacion imp@(Importacion {comentarioImp = Just c})    = intercalate "\n" (map ("-- " ++) (lineas c)) ++ "\n" ++ showImportacion (imp {comentarioImp = Nothing})
showImportacion imp@(Importacion {nombreImp = nI})            = "import " ++ showNombre (NM nI) ++ "\n"


instance Show Data where
    show = showData

showData :: Data -> String
showData d@(Data {comentarioDat = Just c})                 = intercalate "\n" (map ("-- " ++) (lineas c)) ++ "\n" ++ showData (d {comentarioDat = Nothing})
showData d@(Data {nombreDat = nomD, definicionDat = defD}) = "data " ++ nomD ++ " = " ++ defD ++ "\n"


instance Show Clase where
    show = showClase

showClase :: Clase -> String
showClase cl@(Clase {comentarioCla = Just c})                = intercalate "\n" (map ("-- " ++) (lineas c)) ++ "\n" ++ showClase (cl {comentarioCla = Nothing})
-- showClase cl@(Clase {herenciaCla = Just herC, nombreCla = nomC})  = "class " ++ herC ++ " => " ++ nomC ++ " where" ++ "\n" ++ showClase (cl {herenciaCla = Nothing})
-- showClase cl@(Clase {nombreCla = nomC})                      = "class " ++ nomC ++ " where \n" ++ showClase (cl {nombreCla = Nothing})    -- TODO: Sirve pasar el nombre a Maybe?
-- showClase cl@(Clase {firmaCla = firmaC, whereCla = whereC})  = firmaC ++ " " ++ showClaseWhere whereC  -- TODO: Hacer. Sirve lo mismo para Función?
-- showClase cl@(Clase {firmaCla = firmaC})                     = firmaC
showClase cl@(Clase {herenciaCla = Just herC, nombreCla = nomC, firmaCla = firmaC, whereCla = whereC})  = "class " ++ herC ++ " => " ++ nomC ++ " where" ++ "\n" ++ firmaC ++ " " ++ showClaseWhere whereC
showClase cl@(Clase {nombreCla = nomC, firmaCla = firmaC, whereCla = whereC}) = "class " ++ nomC ++ " where \n" ++ firmaC ++ " " ++ showClaseWhere whereC

showClaseWhere _ = "TODO: Hacer showClaseWhere"

instance Show Instancia where
    show = showInstancia

showInstancia :: Instancia -> String
showInstancia inst@(Instancia {comentarioIns = Just c})                  = intercalate "\n" (map ("-- " ++) (lineas c)) ++ "\n" ++ showInstancia (inst {comentarioIns = Nothing})
-- showInstancia inst@(Instancia {nombreIns = nomI, nombreDatoIns = nomDI}) = "instance " ++ nomI ++ " " ++ nomDI ++ " where \n" ++ showInstancia (inst {nombreIns = Nothing})    -- TODO: Sirve pasar el nombre a Maybe?
-- showInstancia inst@(Instancia {whereIns = whereInst})                    = showInstanciaWhere whereInst  -- TODO: Hacer. Sirve lo mismo para Función?
showInstancia inst@(Instancia {nombreIns = nomI, nombreDatoIns = nomDI, whereIns = whereInst}) = "instance " ++ nomI ++ " " ++ nomDI ++ " where \n" ++ showInstanciaWhere whereInst  -- TODO: Hacer. Sirve lo mismo para Función?

showInstanciaWhere _ = "TODO: Hacer showInstanciaWhere"


{- TODO:
- Hacer showClaseWhere. Se puede reutilizar para Funcion e Instance (ver definición. Por qué no es maybe Where?) 
- Terminar showFuncion
-}


instance Show Funcion where
    show = showFuncion

showFuncion :: Funcion -> String
showFuncion f@(Funcion {comentarioFun = Just c})                   = intercalate "\n" (map ("-- " ++) (lineas c)) ++ "\n" ++ showFuncion (f {comentarioFun = Nothing})
showFuncion f@(Funcion {nombreFun = nomF, firmaFun = Just miFirma})  = (showNombre (NF nomF)) ++ " :: " ++ miFirma ++ "\n" ++ showFuncion (f {firmaFun = Nothing})
showFuncion f@(Funcion {patronesFun = []})                         = ""
showFuncion f@(Funcion {nombreFun = nomF, patronesFun = (p:ps)})     = showFuncionPatron' (showNombre (NF nomF)) p ++ "\n" ++ showFuncion (f {patronesFun = ps})

-- showFuncionPatron f@(Funcion {patronesFun = []})                   = "\n"
-- showFuncionPatron f@(Funcion {nombreFun = nomF, patronesFun = (p:ps)}) = showFuncionPatron' nomF p ++ "\n" ++ showFuncionPatron (f {patronesFun = ps})

showFuncionPatron' :: nombreFun -> Patron -> String
-- showFuncionPatron' nomF p@(Patron {comentarioPat = Nothing})       = showFuncionPatronArg nomF p
showFuncionPatron' nomF p@(Patron {comentarioPat = Nothing})       = showFuncionPatronPipes p

-- showFuncionPatronArg :: nombreFun -> Patron -> String
-- showFuncionPatronArg nomF p@(Patron {argumentosPt = a})            = nomF ++ " " ++ a ++ showFuncionPatronPipes p

{-
showFuncionPatronPipes :: Patron -> String
showFuncionPatronPipes p@(Patron {blopipePat = Left (Expresion bloque c)}) = " = " ++ bloque ++ showFuncionPatronWhere p
showFuncionPatronPipes p@(Patron {blopipePat = Right []}) = ""
showFuncionPatronPipes p@(Patron {blopipePat = Right (Pipe condix (Expresion bloque _):pis)}) = "\n    | " ++ condix ++ " = " ++ bloque ++ showFuncionPatronPipes (p {blopipePat = Right pis})

showFuncionPatronWhere _ = "TODO: Hacer showFuncionPatronWhere"
-}

showFuncionPatronPipes :: Patron -> String
showFuncionPatronPipes p@(Patron {blopipePat = Left (Expresion bloque)}) = " = " ++ bloque
showFuncionPatronPipes p@(Patron {blopipePat = Right []}) = ""
showFuncionPatronPipes p@(Patron {blopipePat = Right (Pipe condix (Expresion bloque):pis)}) = "\n    | " ++ condix ++ " = " ++ bloque ++ showFuncionPatronPipes (p {blopipePat = Right pis})

