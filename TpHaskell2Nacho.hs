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