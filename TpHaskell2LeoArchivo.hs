module TpHaskell2LeoArchivo where

import LoadFile
import TpHaskell2Estructuras
import TpHaskell2Funciones
import Debug.Trace


{-
    FUNCIONES PARA LECTURA DEL ARCHIVO
-}

{-
    Lectura de comentario
-}

esCodigo :: TipoCodigo -> Bool
esCodigo (TCCod _) = True
esCodigo _          = False

tipoCodigoAChr :: TipoCodigo -> Char
tipoCodigoAChr (TCCod x) = x
tipoCodigoAChr (TCComIL x) = x
tipoCodigoAChr (TCComML x) = x

lineas :: String -> [String]
lineas ""           = []
lineas xs
    | null xs'      = [xs]
    | otherwise     = l : lineas (tail xs')
    where (l,xs') = break (=='\n') xs

filtroNulas :: [[a]] -> [[a]]
filtroNulas = filter (not . null) -- comentario

maybeConcateno :: String -> Maybe String -> Maybe String
maybeConcateno c' Nothing = Just c'
maybeConcateno c' (Just c) = Just $ c ++ "\n" ++ c'

data Estoy = Normal | CML | CIL | Comillas
data TipoCodigo = TCComIL Char | TCComML Char | TCCod Char deriving (Show)

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

-- Descomentar esto para probar
--archivoCargado = loadFile $ "/{fileroute}/ArchivoPrueba.hs"

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

tails                   :: [a] -> [[a]]
tails []                =  [[]]
tails xxs@(_:xs)        =  xxs : tails xs

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

-- esto se puede acortar
-- TipoPartCodigo = TipoPartCodigo TPTipo
-- data TpTipo = TPCNomMod
data TipoPartCodigo = 
    TPCNomMod String | TPCImport String | 
    TPCData String | TPCClase String | 
    TPCInsta String | TPCFuncion String | 
    TPCCIL String | TPCCML String | 
    TPCImportCIL String | TPCImportCML String |
    TPCDataCIL String | TPCDataCML String |
    TPCClaseCIL String | TPCClaseCML String |
    TPCInstaCIL String | TPCInstaCML String |
    TPCFuncionCIL String | TPCFuncionCML String |
    TPCFuncionInsta String | TPCFuncionClase String deriving (Show)
data EstoyPartCod = CNormal | CNMod | CImport | CData | CClase | CInsta | CFuncion

-- Detecto Tipo de Codigo
dCod :: [TipoCodigoAg] -> [TipoPartCodigo]
dCod a = dCod' CNormal a
-- Separo cada Cod o Comentario en algo mas especifico, tipo de comentario o de codigo
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
dCod' CClase ((TCACod x):xs)               = (TPCFuncionClase x : dCod' CClase xs)
dCod' CInsta ((TCACod x):xs)               = (TPCFuncionInsta x : dCod' CInsta xs)
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
esFuncion a = not (tieneIndentado a) && (tieneDosPuntos || tieneIgual)
   where tieneDosPuntos = case buscar " :: " a of
                            Just (_) -> True
                            Nothing  -> False
         tieneIgual     = case buscar " = " a of
                            Just (_) -> True
                            Nothing  -> False

tieneIndentado a = esPrefijoDe " " a

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

-- Descomentar esto para probar
--pruebaArchivoALista = quitarQuizas $ (aC <$> dCom archivoCargado)

----

-- Armo el archivo con toda la lista de partes de codigo
armarArchivo :: [TipoPartCodigo] -> Archivo -> Archivo
--armarArchivo (_:[]) ar                = ar 
armarArchivo ((TPCNomMod nmod):[]) ar = ar {nombreArc = (strToNomM nmod)}
armarArchivo ((TPCNomMod nmod):xs) ar = armarArchivo xs (ar {nombreArc = (strToNomM nmod)})
armarArchivo ((TPCImport imp):[]) ar  = ar {importsArc = ((importsArc ar) ++ [(armarImport imp [])])}
armarArchivo ((TPCImport imp):xs) ar  = armarArchivo xs (ar {importsArc = ((importsArc ar) ++ [(armarImport imp xs)])})
armarArchivo ((TPCData dat):[]) ar    = ar {datasArc = ((datasArc ar) ++ [(armarData dat [])])}
armarArchivo ((TPCData dat):xs) ar    = armarArchivo xs (ar {datasArc = ((datasArc $ ar) ++ [(armarData dat xs)])})
armarArchivo ((TPCInsta ins):[]) ar   = ar {instancesArc = ((instancesArc ar) ++ [(armarInstance ins [])])}
armarArchivo ((TPCInsta ins):xs) ar   = armarArchivo xs (ar {instancesArc = ((instancesArc ar) ++ [(armarInstance ins xs)])})
armarArchivo ((TPCClase cla):[]) ar   = ar {clasesArc = ((clasesArc ar) ++ [(armarClase cla [])])}
armarArchivo ((TPCClase cla):xs) ar   = armarArchivo xs (ar {clasesArc = ((clasesArc ar) ++ [(armarClase cla xs)])})
-- aca miro que no tenga indentado, asi me aseguro que es una cabecera de funcion y no parte de otra
armarArchivo ((TPCFuncion fun):_) ar | trace ("armarArchivo <<" ++ fun ++ ">>") False = undefined
armarArchivo ((TPCFuncion fun):[]) ar | not (tieneIndentado fun) = let (func,resto) = armarFuncion fun [] in
                                                                      ar {funcionesArc = ((funcionesArc ar) ++ [func])}
armarArchivo ((TPCFuncion fun):xs) ar | not (tieneIndentado fun) = let (func,resto) = armarFuncion fun ((TPCFuncion fun):xs) in 
                                                                      armarArchivo resto (ar {funcionesArc = funcionesArc ar ++ [func]})
armarArchivo (_:[]) ar                = ar 
armarArchivo (_:xs) ar                = armarArchivo xs ar
armarArchivo [] ar = ar


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

armarInstance :: String -> [TipoPartCodigo] -> Instancia
armarInstance ins ar = armarInstance' ar (Instancia (strToNombreIns ins) (strToNombreDatoIns ins) [] Nothing)

armarInstance' :: [TipoPartCodigo] -> Instancia -> Instancia
armarInstance' ((TPCInstaCIL com):xs) nuevaIns = armarInstance' xs (nuevaIns {comentarioIns = Just com})
armarInstance' ((TPCInstaCML com):xs) nuevaIns = armarInstance' xs (nuevaIns {comentarioIns = Just com})
armarInstance' ((TPCFuncionInsta ins):xs) nuevaIns = let (resp,ar) = armarFuncion ins xs in 
    armarInstance' ar (nuevaIns {whereIns = (whereIns $ nuevaIns) ++ [resp]})
armarInstance' (_) nuevaIns = nuevaIns

strToNombreIns :: String -> String
strToNombreIns ins = getIndexFromList (separo ins) 1

strToNombreDatoIns :: String -> String
strToNombreDatoIns ins = getIndexFromList (separo ins) 2

---

armarClase :: String -> [TipoPartCodigo] -> Clase
armarClase cla ar = armarClase' ar (Clase (strToHerenciaCla cla) (strToNombreCla cla) (strToFirmaCla cla) [] Nothing)

armarClase' :: [TipoPartCodigo] -> Clase -> Clase
armarClase' ((TPCClaseCIL com):xs) nuevaCla = armarClase' xs (nuevaCla {comentarioCla = Just com})
armarClase' ((TPCClaseCML com):xs) nuevaCla = armarClase' xs (nuevaCla {comentarioCla = Just com})
armarClase' ((TPCFuncionClase cla):xs) nuevaCla = let (resp, ar) = armarFuncion cla xs in
    armarClase' ar (nuevaCla {whereCla = (whereCla $ nuevaCla) ++ [resp]})
armarClase' (_) nuevaCla = nuevaCla

strToHerenciaCla :: String -> Maybe String
strToHerenciaCla cla = case buscar " => " cla of
                      Just (ini,fin) -> case buscar " (" ini of
                                        Just (i,f) -> Just f
                                        Nothing -> Nothing
                      Nothing        -> Nothing

strToNombreCla :: String -> String
strToNombreCla cla | (strToHerenciaCla cla) == Nothing = getIndexFromList (separo cla) 1
strToNombreCla cla = case buscar " => " cla of
                      Just (ini,fin) -> case buscar " where" fin of
                                        Just (i,f) -> (tails i) !! 3
                                        Nothing -> ""
                      Nothing        -> ""

strToFirmaCla :: String -> String -- Esto tal vez sacarlo, unificar nombre y firma en uno
strToFirmaCla cla = ""

---

-- Aca armo la funcion, recibo la primera linea de la funcion y toda la lista que sigue
-- para poder agregar los comentarios y los patrones
armarFuncion :: String -> [TipoPartCodigo] -> (Funcion,[TipoPartCodigo])
armarFuncion fun (x:xs) = armarFuncion' xs (Funcion (nombreFuncion fun) (firmaFuncion fun) [] Nothing)
--armarFuncion fun _ = armarFuncion' [] (Funcion (nombreFuncion fun) (firmaFuncion fun) [] Nothing)

armarFuncion' :: [TipoPartCodigo] -> Funcion -> (Funcion,[TipoPartCodigo])
armarFuncion' ((TPCFuncion fun):xs) nuevaFun | trace ("armarFuncion' <<< " ++ fun ++ "::::" ++ nombreFtoStr (nombreFun nuevaFun) ++ ">>>>") False = undefined
armarFuncion' ((TPCFuncionCIL com):xs) nuevaFun = armarFuncion' xs (nuevaFun {comentarioFun = Just com})
armarFuncion' ((TPCFuncionCML com):xs) nuevaFun = armarFuncion' xs (nuevaFun {comentarioFun = Just com})
--armarFuncion' ((TPCFuncionInsta fun):xs) nuevaFun = armarFuncion' xs (nuevaFun {whereFun = (whereFun $ nuevaFun) ++ [(armarFuncion fun xs)]})
--armarFuncion' ((TPCFuncionClase fun):xs) nuevaFun = armarFuncion' xs (nuevaFun {whereFun = (whereFun $ nuevaFun) ++ [(armarFuncion fun xs)]})

-- aca miro que haya indentado y el indentado de la proxima funcion sea mayor al de la linea actual, si es asi debe ser patron de esta
armarFuncion' ((TPCFuncion fun):xs) nuevaFun | (tieneIndentado fun) && ((indentado fun) > (indentado (nombreFtoStr(nombreFun nuevaFun)))) = agregarPatrones nuevaFun fun xs
-- aca miro que tenga indentado, pero no es patron, es decir tiene el mismo indentado o menor
armarFuncion' ((TPCFuncion fun):xs) nuevaFun | (tieneIndentado fun) = armarFuncion' xs (nuevaFun {patronesFun = patronesFun (nuevaFun) ++ [crearPatron fun]})
armarFuncion' ar nuevaFun = (nuevaFun, ar)

indentado :: String -> Int
indentado str = indentado' str 0

indentado' :: String -> Int -> Int
indentado' (' ':xs) conteo = indentado' xs (succ conteo)
indentado' (_) conteo = conteo

trace_show s x = trace (s ++ ":" ++ show x) x

-- Aca agrego los patrones de cada funcion, recibo la funcion padre y el primer patron y el resto de la lista
agregarPatrones :: Funcion -> String -> [TipoPartCodigo] -> (Funcion, [TipoPartCodigo])
--agregarPatrones funPadre patron xs | ((indentado patron) == (indentado (nombreFtoStr(nombreFun funPadre)))) = funPadre{patronesFun = (patronesFun $ funPadre) ++ [crearPatron patron]} 
-- agregarPatrones funPadre patron (TPCFuncion proxFun:[]) = funPadre{patronesFun = (patronesFun $ funPadre) ++ [crearPatron patron]}
agregarPatrones funPadre patron xs | trace ("agregarPatrones <<<" ++ nombreFtoStr (nombreFun funPadre) ++ ">>>") False = undefined
agregarPatrones funPadre patron xs | indentado (patron) > indentado (nombreFtoStr (nombreFun funPadre)) = let (pats, ar) = agregarPatronesRecursivo (crearPatron patron) xs in
   (funPadre{patronesFun = (patronesFun $ funPadre) ++ [pats]}, ar)
--agregarPatrones funPadre patron xs = let (pats, ar) = agregarPatronesRecursivo (crearPatron patron) xs in
--    (funPadre{patronesFun = (patronesFun $ funPadre) ++ [pats]}, ar)
agregarPatrones funPadre _ ar = (funPadre,ar)

-- Aca me fijo si debo agregar un where dentro del patron
agregarPatronesRecursivo :: Patron -> [TipoPartCodigo] -> (Patron, [TipoPartCodigo])
agregarPatronesRecursivo patron (TPCFuncion proxFun:rest) | trace ("agregarPatronesRecursivo : Funcion <<" ++ proxFun ++ ">> : FuncionPatron <<" ++ maybeNombreFuncion(ultimoWherePat patron) ++ ">>") False = undefined

-- si el indentado de la proxima funcion es mayor al de la anterior, agrego al where
agregarPatronesRecursivo patron (TPCFuncion proxFun:rest) | (indentado proxFun) > (indentado (maybeNombreFuncion(ultimoWherePat patron))) = 
    case (ultimoWherePat patron) of
      Just f | trace ("agregarPatronesRecursivo JUST : Funcion <<" ++ nombreFtoStr (nombreFun f) ++ ">> : FuncionPatron <<" ++ maybeNombreFuncion(ultimoWherePat patron) ++ ">>") False -> undefined
      Nothing | trace ("agregarPatronesRecursivo NOTHING : Funcion <<"++ proxFun ++ ">> : FuncionPatron <<" ++ maybeNombreFuncion(ultimoWherePat patron) ++ ">>") False -> undefined
      Just f  -> let (resp, ar) = agregarPatrones f proxFun rest in
          (patron {wherePat = (wherePat $ patron) ++ [resp]}, ar)
      Nothing -> let (resp, ar)        = armarFuncion proxFun (TPCFuncion proxFun:rest) 
                     (respPatr, arPat) = agregarPatrones (resp) proxFun ar in
                       (patron {wherePat = [respPatr]}, arPat) -- no deberia ir ar aca?
                       

-- agregarPatronesRecursivo patron fun _ = crearPatron fun
agregarPatronesRecursivo patron rest = (patron, rest)

-- esto busca dentro de los where de un patron, el ultimo
ultimoWherePat :: Patron -> Maybe Funcion
ultimoWherePat patron = ultimoWherePat' (wherePat $ patron)

ultimoWherePat' :: [Funcion] -> Maybe Funcion
ultimoWherePat' ([])   = Nothing
ultimoWherePat' (x:[]) = Just x
ultimoWherePat' (_:xs) = ultimoWherePat' xs

nombreFtoStr :: NombreF -> String
nombreFtoStr (NombreF nom) = nom

maybeNombreFuncion :: Maybe Funcion -> String
maybeNombreFuncion (Just f) = nombreFtoStr $ nombreFun $ f
maybeNombreFuncion Nothing = ""

---
crearPatron linea = Patron (Left (Expresion linea)) [] Nothing

nombreFuncion :: String -> NombreF
nombreFuncion fun = NombreF fun -- habría ver que querriamos hacer aca

firmaFuncion :: String -> Maybe String
firmaFuncion fun = case buscar "::" fun of
                      Just (ini,(':':':':fin)) -> Just fin
                      Nothing                  -> Nothing




----
archivoVacio :: Archivo
archivoVacio = Archivo (NombreM "") [] [] [] [] []

-- Descomentar esto para probar
--archivoArmado = armarArchivo (dCod $ pruebaArchivoALista) archivoVacio











