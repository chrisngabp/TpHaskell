module TpHaskell2MuestroArchivo where

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


-- Ignacio Segura:

instance Show Archivo where
    show = showArchivo

{-
-- data Opciones = Opciones {largoLinea :: Int, anchoTab :: Int, comentarioInline :: Bool, compacto :: Bool, colores :: Bool} -- Pueden agregar otros parámetros.

largoLinea: corta las líneas cada x caracteres, pero no podemos cortar en medio de una palabra. 
    Una barra "\" indica la continuación de la línea anterior.
anchoTab: determina el ancho de los tabs. 1 espacio, 2 espacios, 3 espacios...
comentarioInline: 
compacto: Borraría los saltos de línea intermedios, siempre y cuando todo siga funcionando. Quedaría visualmente feo.
colores: activa colores para lo que definamos. Es más avanzado.
-}

-- defaultOpciones :: Opciones




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

