module TpHaskell2Index where

import LoadFile
import TpHaskell2Estructuras

{-
-
- INICIO DE FUNCIONES
-
-}

-- Generales
esCodigo :: TipoCodigo -> Bool
esCodigo (TCCod _) = True
esCodigo _          = False

tipoCodigoAChr :: TipoCodigo -> Char
tipoCodigoAChr (TCCod x) = x
tipoCodigoAChr (TCComIL x) = x
tipoCodigoAChr (TCComML x) = x

fromQuizas (OK x) = x

lineas :: String -> [String]
lineas ""           = []
lineas xs
    | null xs'      = [xs]
    | otherwise     = l : lineas (tail xs')
    where (l,xs') = break (=='\n') xs

filtroNulas :: [[a]] -> [[a]]
filtroNulas = filter (not . null) -- comentario
data Estoy = Normal | CML | CIL | Comillas
data TipoCodigo = TCComIL Char | TCComML Char | TCCod Char deriving (Show)

maybeConcateno :: String -> Maybe String -> Maybe String
maybeConcateno c' Nothing = Just c'
maybeConcateno c' (Just c) = Just $ c ++ "\n" ++ c'

-- Comentarios
data Comentado a = Comentado String a

comentarioAString :: Comentario -> String
comentarioAString Nothing = ""
comentarioAString (Just x) = x

stringAComentario :: String -> Comentario
stringAComentario [] = Nothing
stringAComentario x = Just x


{-
    FUNCIONES PARA LECTURA DEL ARCHIVO
-}

{-
    Lectura de comentario
-}

dCom :: String -> Quizas [TipoCodigo]
dCom xs                 = dCom' Normal xs --

dCom' :: Estoy -> String -> Quizas [TipoCodigo]
dCom' CML []                 = Error "CML no terminado"
dCom' Comillas []            = Error "Comillas no cerradas"
dCom' _ []                   = OK []
dCom' Normal ('{':'-':xs)    = dCom' CML xs
dCom' CML ('-':'}':xs)       = dCom' Normal xs
dCom' CML (x:xs)             = (TCComML x :) <$> dCom' CML xs
dCom' CIL ('\n':xs)          = (TCCod '\n' :) <$> dCom' Normal xs
dCom' Normal ('-':'-':xs)    = dCom' CIL xs
dCom' CIL (x:xs)             = (TCComIL x :) <$> dCom' CIL xs
dCom' Normal ('"':xs)        = (TCCod '"' :) <$> dCom' Comillas xs
dCom' Comillas ('"':xs)      = (TCCod '"' :) <$> dCom' Normal xs
dCom' Normal (x:xs)          = (TCCod x :) <$> dCom' Normal xs
dCom' Comillas (x:xs)        = (TCCod x :) <$> dCom' Comillas xs

-- si recibo un /" tengo que tenerlo en  cuenta

-- agrupo comentario
data TipoCodigoAg = TCACod String | TCAComIL String | TCAComML String deriving (Show)

aC :: [TipoCodigo] -> [TipoCodigoAg]
aC []               = []
aC xs'@(TCCod x:xs) = map TCACod linIni ++ aC fin
    where (ini,fin) = break (not . esCodigo) xs'
          ini'      = map tipoCodigoAChr ini
          linIni    = filter (not . null) $ lineas ini'
          -- esEnter = last ini
          -- esCILDespues = head fin
aC xs'@(TCComIL x:xs) = TCAComIL (map tipoCodigoAChr ini) : aC fin
    where (ini,fin) = break esCodigo xs'
aC xs'@(TCComML x:xs) = TCAComML (map tipoCodigoAChr ini) : aC fin
    where (ini,fin) = break esCodigo xs'
-- Ver que pasa si nos agregan un comentario inline como un multiline

data TipoCodigoAg2 = TCACod2 String Comentario deriving (Show)

aC2 :: [TipoCodigoAg] -> [TipoCodigoAg2]
aC2 (TCAComML x:TCACod y:TCAComIL x':xs) = TCACod2 y (Just (x ++ "\n" ++ x')) : aC2 xs
aC2 (TCAComML x:TCACod y:xs) = TCACod2 y (Just x) : aC2 xs
aC2 (TCACod y:TCAComIL x:xs) = TCACod2 y (Just x) : aC2 xs
aC2 (TCAComIL x:TCAComML y:xs) = aC2 (TCAComML (x ++ "\n" ++ y) : xs)
aC2 (TCAComML x:TCAComIL y:xs) = aC2 (TCAComML (x ++ "\n" ++ y) : xs)
aC2 (TCACod x:xs) = TCACod2 x Nothing : aC2 xs
aC2 (_:xs)             = aC2 xs
aC2 []                 = []

maybeConcatenoComentario :: String -> Maybe String -> Maybe String
maybeConcatenoComentario c' Nothing = Just c'
maybeConcatenoComentario c' (Just c) = Just $ c ++ "\n" ++ c'

-- Para probar esto -> aC <$> dCom "string con cosas"

archivoCargado = loadFile $ "/Users/cpanetta/Desktop/Fracciones.hs"

-- Esto lo uso para probar

{-
    Lectura de Codigos
-}

esPrefijoDe :: Eq a => [a] -> [a] -> Bool
esPrefijoDe [] _ = True
esPrefijoDe _ [] = False
esPrefijoDe (x:xs) (y:ys) = x == y && esPrefijoDe xs ys

buscar :: Eq a => [a] -> [a] -> Maybe ([a], [a])
buscar xs ys   | esPrefijoDe xs ys = Just ([],ys)
buscar _ []                        = Nothing
buscar xs (y:ys)                   = agrego <$> buscar xs ys
    where agrego (as,bs) = (y:as,bs)
          g = 10

separo :: String -> [String]
separo str = case dropWhile esEspacio str of
                "" -> []
                str' -> w : separo str''
                        where (w, str'') =
                                break esEspacio str'

esEspacio :: Char -> Bool
esEspacio ' ' = True
esEspacio _ = False

cantEspacios :: String -> Int
cantEspacios (' ':xs) = succ $ cantEspacios xs
cantEspacios _        = 0

data Infinito a = Infinito [(a,Infinito a)] deriving (Show)

agrupoLinea :: [String] -> Infinito String
agrupoLinea []      = Infinito []
agrupoLinea (x:xs)  = Infinito ((x,agrupoLinea ini) : fin')
    where n = cantEspacios x
          (ini,fin) = break ((<=n) . cantEspacios) xs
          Infinito fin' = agrupoLinea fin

quitarQuizas :: Quizas a -> a
quitarQuizas (OK a) = a

tipoCodigoAgAStr :: TipoCodigoAg -> String
tipoCodigoAgAStr (TCACod x) = x
tipoCodigoAgAStr (TCAComIL x) = x
tipoCodigoAgAStr (TCAComML x) = x

tipoPartCodigoAgAStr :: TipoPartCodigo -> String
tipoPartCodigoAgAStr (TPCNomMod x) = x
tipoPartCodigoAgAStr (TPCImport x) = x
tipoPartCodigoAgAStr (TPCData x) = x
tipoPartCodigoAgAStr (TPCInsta x) = x
tipoPartCodigoAgAStr (TPCFuncion x) = x
tipoPartCodigoAgAStr (TPCImportCIL x) = x
tipoPartCodigoAgAStr (TPCImportCML x) = x
tipoPartCodigoAgAStr (TPCDataCIL x) = x
tipoPartCodigoAgAStr (TPCDataCML x) = x
tipoPartCodigoAgAStr (TPCClaseCIL x) = x
tipoPartCodigoAgAStr (TPCClaseCML x) = x
tipoPartCodigoAgAStr (TPCInstaCIL x) = x
tipoPartCodigoAgAStr (TPCInstaCML x) = x
tipoPartCodigoAgAStr (TPCFuncionCIL x) = x
tipoPartCodigoAgAStr (TPCFuncionCML x) = x

data TipoPartCodigo = 
    TPCNomMod String | TPCImport String | 
    TPCData String | TPCClase String | 
    TPCInsta String | TPCFuncion String | 
    TPCCIL String | TPCCML String | 
    TPCImportCIL String | TPCImportCML String |
    TPCDataCIL String | TPCDataCML String |
    TPCClaseCIL String | TPCClaseCML String |
    TPCInstaCIL String | TPCInstaCML String |
    TPCFuncionCIL String | TPCFuncionCML String deriving (Show)
data EstoyPartCod = CNormal | CNMod | CImport | CData | CClase | CInsta | CFuncion

-- Detecto Tipo de Codigo
dCod :: [TipoCodigoAg] -> [TipoPartCodigo]
dCod a = dCod' CNormal a

dCod' :: EstoyPartCod -> [TipoCodigoAg] -> [TipoPartCodigo]
dCod' _ []                                 = []
dCod' CNormal ((TCAComIL x):xs)            = (TPCCIL x : dCod' CNormal xs)
dCod' CNormal ((TCAComML x):xs)            = (TPCCML x : dCod' CNormal xs)
dCod' CImport ((TCAComIL x):xs)            = (TPCImportCIL x : dCod' CImport xs)
dCod' CImport ((TCAComML x):xs)            = (TPCImportCML x : dCod' CImport xs)
dCod' CData ((TCAComIL x):xs)              = (TPCDataCIL x : dCod' CData xs)
dCod' CData ((TCAComML x):xs)              = (TPCDataCML x : dCod' CData xs)
dCod' CClase ((TCAComIL x):xs)             = (TPCClaseCIL x : dCod' CClase xs)
dCod' CClase ((TCAComML x):xs)             = (TPCClaseCML x : dCod' CClase xs)
dCod' CInsta ((TCAComIL x):xs)             = (TPCInstaCIL x : dCod' CInsta xs)
dCod' CInsta ((TCAComML x):xs)             = (TPCInstaCML x : dCod' CInsta xs)
dCod' CFuncion ((TCAComIL x):xs)           = (TPCFuncionCIL x : dCod' CFuncion xs)
dCod' CFuncion ((TCAComML x):xs)           = (TPCFuncionCML x : dCod' CFuncion xs)
dCod' _ ((TCACod x):xs) | esNombreModulo x = (TPCNomMod x : dCod' CNormal xs)
dCod' _ ((TCACod x):xs) | esImport x       = (TPCImport x : dCod' CImport xs)
dCod' _ ((TCACod x):xs) | esData x         = (TPCData x : dCod' CData xs)
dCod' _ ((TCACod x):xs) | esClase x        = (TPCClase x : dCod' CClase xs)
dCod' _ ((TCACod x):xs) | esInstancia x    = (TPCInsta x : dCod' CInsta xs)
dCod' _ ((TCACod x):xs) | esFuncion x      = (TPCFuncion x : dCod' CFuncion xs)
dCod' CData ((TCACod x):xs)                = (TPCData x : dCod' CData xs)
dCod' CClase ((TCACod x):xs)               = (TPCClase x : dCod' CClase xs)
dCod' CInsta ((TCACod x):xs)               = (TPCInsta x : dCod' CInsta xs)
dCod' CFuncion ((TCACod x):xs)             = (TPCFuncion x : dCod' CFuncion xs)

esNombreModulo :: String -> Bool
esNombreModulo a = tieneModule && tieneWhere
    where tieneModule = esPrefijoDe "module " a
          tieneWhere  = case buscar " where" a of
                          Just (ini,fin) -> fin == " where"
                          Nothing        -> False

esImport :: String -> Bool
esImport a = tieneImport
   where tieneImport = esPrefijoDe "import " a

esFuncion :: String -> Bool
esFuncion a = not tieneIndentado && (tieneDosPuntos || tieneIgual)
   where tieneDosPuntos = case buscar " :: " a of
                            Just (_) -> True
                            Nothing  -> False
         tieneIgual     = case buscar " = " a of
                            Just (_) -> True
                            Nothing  -> False
         tieneIndentado = esPrefijoDe " " a

esData :: String -> Bool
esData a = tieneData && tieneIgual
   where tieneData      = esPrefijoDe "data " a
         tieneIgual     = case buscar " = " a of
                            Just (ini, fin) -> True
                            Nothing         -> False

esClase :: String -> Bool
esClase a = tieneClase && tieneWhere
   where tieneClase     = esPrefijoDe "class " a
         tieneWhere  = case buscar " where" a of
                            Just (ini,fin) -> fin == " where"
                            Nothing        -> False

esInstancia :: String -> Bool
esInstancia a = tieneInstancia && tieneWhere
   where tieneInstancia = esPrefijoDe "instance " a
         tieneWhere  = case buscar " where" a of
                            Just (ini,fin) -> fin == " where"
                            Nothing        -> False

pruebaArchivoALista = quitarQuizas $ (aC <$> dCom archivoCargado)

----

armarArchivo :: [TipoPartCodigo] -> Archivo -> Archivo
--armarArchivo (_:[]) ar                = ar 
armarArchivo ((TPCNomMod nmod):[]) ar = ar {nombreArc = (strToNomM nmod)}
armarArchivo ((TPCNomMod nmod):xs) ar = armarArchivo xs (ar {nombreArc = (strToNomM nmod)})
armarArchivo ((TPCImport imp):[]) ar  = ar {importsArc = ((imports ar) ++ [(armarImport imp [])])}
armarArchivo ((TPCImport imp):xs) ar  = armarArchivo xs (ar {importsArc = ((imports ar) ++ [(armarImport imp xs)])})
armarArchivo ((TPCData dat):[]) ar    = ar {datasArc = ((datas ar) ++ [(armarData dat [])])}
armarArchivo ((TPCData dat):xs) ar    = armarArchivo xs (ar {datasArc = ((datas ar) ++ [(armarData dat xs)])})
armarArchivo (_:[]) ar                = ar 
armarArchivo (_:xs) ar                = armarArchivo xs ar


--
strToNomM :: String -> NombreM
strToNomM str = NombreM (getIndexFromList (separo str) 1)
--

getIndexFromList :: [a] -> Int -> a
getIndexFromList (x:xs) 0 = x
getIndexFromList (x:xs) i = getIndexFromList xs (i - 1)

--
armarImport :: String -> [TipoPartCodigo] -> Importacion
armarImport nom ((TPCImportCIL x):_) = Importacion (strToNomM nom) (Just x)
armarImport nom ((TPCImportCML x):_) = Importacion (strToNomM nom) (Just x)
armarImport nom (_) = Importacion (strToNomM nom) Nothing
--
armarData :: String -> [TipoPartCodigo] -> Data
armarData dat ((TPCDataCIL com):_) = Data (dataNombre dat) (dataDefinicion dat) (Just com)
armarData dat ((TPCDataCML com):_) = Data (dataNombre dat) (dataDefinicion dat) (Just com)
armarData dat (_) = Data (dataNombre dat) (dataDefinicion dat) Nothing

dataNombre :: String -> String 
dataNombre str = getIndexFromList (separo str) 1

dataDefinicion :: String -> String
dataDefinicion str = case buscar " = " str of
                            Just (ini,(' ':'=':' ':fin)) -> fin
                            Nothing        -> ""

----
archivoVacio :: Archivo
archivoVacio = Archivo (NombreM "") [] [] [] [] []

archivoArmado = armarArchivo (dCod $ pruebaArchivoALista) archivoVacio













{-

ACA NO MIRO NADA

-}

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

clasePruebaClases = Clase (Just "(Monad m, Monad (t m))") "Transform" "t m" Nothing Nothing
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
patronFuncionPrueba = Patron (Left bloqueFuncionPrueba1) Nothing Nothing
patronFuncionPrueba2 = Patron (Left bloqueFuncionPrueba2 ) Nothing (Just "-- comento esto")
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
