module TpHaskell2Index where

import LoadFile
import TpHaskell2Estructuras
import TpHaskell2LeoArchivo

{-
-
- INICIO DE FUNCIONES
-
-}

-- Esto lo uso para probar

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

dataPruebaData = Data "Estoy" "= Normal | CML | CIL | Comillas" Nothing
archivoPruebaDatas = Archivo (NombreM "") [] [dataPruebaData] [] [] []
datas :: Archivo -> [Data] -- Tipos de dato que genera
datas (Archivo _ _ d _ _ _) = d
-- para probar en GHCI : datas $ archivoPruebaDatas

clasePruebaClases = Clase (Just "(Monad m, Monad (t m))") "Transform" "t m" [] Nothing
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

data Archivo = Archivo {nombreArc :: NombreM, importsArc :: [Importacion], datasArc :: [Data], clasesArc :: [Clase], instancesArc :: [Instancia], funcionesArc :: [Funcion]} deriving (Show)

instance Show Archivo where
    show = showArchivo

-- showArchivo :: Opciones -> Archivo -> String -- Muestra el archivo con formato
showArchivo :: Archivo -> String 
showArchivo a@(Archivo {nombreArc = nA})                = "module " ++ nA ++ " where" ++ "\n" ++ showArchivo(a {nombreArc = nA})    -- TODO: Ver cómo no volver a entrar en esta línea

-- showArchivo a@(Archivo {importsArc = []})            = ""
showArchivo a@(Archivo {importsArc = (imp:imps)})       = showImportacion imp ++ "\n" ++ showArchivo(a {importsArc = imps})

-- showArchivo a@(Archivo {datasArc = []})              = ""
showArchivo a@(Archivo {datasArc = (d:ds)})             = showData d ++ "\n" ++ showArchivo (a {datasArc = ds})

-- showArchivo a@(Archivo {clasesArc = []})             = ""
showArchivo a@(Archivo {clasesArc = (c:cs)})            = showClase c ++ "\n" ++ showArchivo (a {clasesArc = cs})

-- showArchivo a@(Archivo {instancesArc = []})          = ""
showArchivo a@(Archivo {instancesArc = (inst:insts)})   = showInstance inst ++ "\n" ++ showArchivo (a {instancesArc = insts})

-- showArchivo a@(Archivo {funcionesArc = []})          = ""
showArchivo a@(Archivo {funcionesArc = (f:fs)})         = showFuncion f ++ "\n" ++ showArchivo (a {funcionesArc = fs})


instance Show Importacion where
    show = showImportacion

showImportacion :: Importacion -> String
showImportacion f@(Importacion {comentarioImp = Just c})    = intercalate "\n" (map ("-- " ++) (lineas c)) ++ "\n" ++ showImportacion (f {comentarioImp = Nothing})
showImportacion f@(Importacion {nombreImp = nI})            = "import " ++ nI ++ "\n"


instance Show Data where
    show = showData

showData :: Importacion -> String
showData f@(Importacion {comentarioDat = Just c})                 = intercalate "\n" (map ("-- " ++) (lineas c)) ++ "\n" ++ showData (f {comentarioDat = Nothing})
showData f@(Importacion {nombreDat = nomD, definicionDat = defD}) = "data " ++ nomD ++ " = " ++ defD ++ "\n"


instance Show Clase where
    show = showClase

showClase :: Clase -> String
showClase c@(Clase {comentarioCla = Just c})                = intercalate "\n" (map ("-- " ++) (lineas c)) ++ "\n" ++ showClase (c {comentarioCla = Nothing})
showClase c@(Clase {herenciaCla = herC, nombreCla = nomC})  = "class " ++ herC ++ " => " ++ nomC ++ " where" ++ "\n" ++ showClase (c {nombreCla = Nothing})
showClase c@(Clase {nombreCla = nomC})                      = "class " ++ nomC ++ " where \n" ++ showClase (c {nombreCla = Nothing})    -- TODO: Sirve pasar el nombre a Maybe?
showClase c@(Clase {firmaCla = firmaC, whereCla = Just whereC})  = firmaC ++ " " ++ showClaseWhere whereC  -- TODO: Hacer. Sirve lo mismo para Función?
showClase c@(Clase {firmaCla = firmaC})                     = firmaC


instance Show Instance where
    show = showInstance

showInstance :: Instance -> String
showInstance c@(Instance {comentarioIns = Just c})                  = intercalate "\n" (map ("-- " ++) (lineas c)) ++ "\n" ++ showInstance (c {comentarioIns = Nothing})
showInstance c@(Instance {nombreIns = nomI, nombreDatoIns = nomDI}) = "instance " ++ nomI ++ nomDI ++ " where \n" ++ showInstance (c {nombreIns = Nothing})    -- TODO: Sirve pasar el nombre a Maybe?
showInstance c@(Instance {whereIns = whereInst})                    = firmaC ++ " " ++ showInstanceWhere whereInst  -- TODO: Hacer. Sirve lo mismo para Función?

{- TODO:
- pasar nombreArchivo a Maybe String ?
- pasar nombreClase a Maybe String ?
- Hacer showClaseWhere. Se puede reutilizar para Funcion e Instance (ver definición. Por qué no es maybe Where?) 
- Terminar showFuncion
-}


instance Show Funcion where
    show = showFuncion

showFuncion :: Funcion -> String
showFuncion f@(Funcion {comentarioFun = Just c})                   = intercalate "\n" (map ("-- " ++) (lineas c)) ++ "\n" ++ showFuncion (f {comentarioFun = Nothing})
showFuncion f@(Funcion {nombreFun = n, firmaFun = Just miFirma})   = n ++ " :: " ++ miFirma ++ "\n" ++ showFuncion (f {firmaFun = Nothing})
showFuncion f@(Funcion {patronesFun = []})                         = ""
showFuncion f@(Funcion {nombreFun = nf, patronesFun = (p:ps)})     = showFuncionPatron' nf p ++ "\n" ++ showFuncion (f {patronesFun = ps})

-- showFuncionPatron f@(Funcion {patronesFun = []})                   = "\n"
-- showFuncionPatron f@(Funcion {nombreFun = nf, patronesFun = (p:ps)}) = showFuncionPatron' nf p ++ "\n" ++ showFuncionPatron (f {patronesFun = ps})

showFuncionPatron' :: nombreFun -> Patron -> String
showFuncionPatron' nf p@(Patron {comentarioPt = Nothing})        = showFuncionPatronArg nf p

showFuncionPatronArg :: nombreFun -> Patron -> String
showFuncionPatronArg nf p@(Patron {argumentosPt = a})            = nf ++ " " ++ a ++ showFuncionPatronPipes p

showFuncionPatronPipes p@(Patron {blopipePat = Left (Expresion bloque c)}) = " = " ++ bloque ++ showFuncionPatronWhere p
showFuncionPatronPipes p@(Patron {blopipePat = Right []}) = ""
showFuncionPatronPipes p@(Patron {blopipePat = Right (Pipe condix (Expresion bloque _):pis)}) = "\n    | " ++ condix ++ " = " ++ bloque ++ showFuncionPatronPipes (p {patron = Right pis})

showFuncionPatronWhere _ = ""
