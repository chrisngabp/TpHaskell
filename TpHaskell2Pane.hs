module TpHaskell2Index where

import LoadFile

data Quizas a = Error String | OK a deriving (Show)

instance Functor Quizas where
    fmap _ (Error x) = Error x
    fmap f (OK x) = OK $ f x

instance Applicative Quizas where
    pure x          = OK x
    _ <*> Error x   = Error x
    Error x <*> _   = Error x
    OK f <*> OK x   = OK $ f x

instance Monad Quizas where
    Error x >>= _ = Error x
    OK x >>= f    = f x

data Archivo = Archivo {nombreArc :: NombreM, importsArc :: [Importacion], datasArc :: [Data], clasesArc :: [Clase], instancesArc :: [Instancia], funcionesArc :: [Funcion]} deriving (Show)

{-
instance Show Archivo where
    show = undefined -- mostrarArchivo
-}

-- instance Eq Archivo where

data Importacion = Importacion {nombreImp :: NombreM, comentarioImp :: Comentario} deriving (Show) -- "Data.List" "import Data.List (inits)"

data Codigo = CD Data | CC Clase | CI Instancia | CF Funcion

data Data = Data {nombreDat :: NombreDeDato, definicionDat :: String} deriving (Show)
type NombreDeDato = String

data Clase = Clase {herenciaCla :: Maybe HerenciaClase, nombreCla :: NombreClase, firmaCla :: Firma, whereCla :: Maybe Where} deriving (Show)
type NombreClase = String
type HerenciaClase = String 

data Instancia = Instancia {nombreIns :: NombreClase, nombreDatoIns :: NombreDeDato, whereIns :: Where} deriving (Show)
type TipoInstancia = String

-- INICIO Datas y types de Funcion

data Funcion = Funcion {nombreFun :: NombreF, firmaFun :: Maybe Firma, patronesFun :: [Patron], comentarioFun :: Comentario} deriving (Show)
type Firma = String -- Sinonimos (type)

-- Falta agregarle los argumentos a Patron Argumentos
data Patron = Patron { blopipePat :: Either Bloque [Pipe], wherePat :: Maybe Where, comentarioPat :: Comentario} deriving (Show)
type Argumentos = String
type Where = [Funcion]
data Bloque =   Expresion Resultado | 
                LetIn [Funcion] Bloque | 
                IfThenElse Bloque Bloque Bloque | 
                Case [Bloque] deriving (Show)

type Resultado = String
type ResultadoBool = String
data Pipe = Pipe ResultadoBool Bloque deriving (Show)

-- FIN Datas y types de Funcion

type Comentario = Maybe String
class ConComentario a where
    comentario :: a -> Maybe Comentario

{-
class ConComentario a where
    comentario :: a -> Maybe Comentario
    agregoComentario :: Comentario -> a -> a
-}

{-
instance ConComentario Funcion where
    agregoComentario c' (Funcion f x w mc) = Funcion f x w (maybeConcateno c' mc)
-}

data NombreM = NombreM String deriving (Show) -- Nombre válido para módulos, datos, etc.
data NombreF = NombreF String | OperadorF String deriving (Show, Eq) -- Nombre válido para funciones
data Nombre = NM NombreM | NF NombreF

data Opciones = Opciones {largoLinea :: Int, anchoTab :: Int, comentarioInline :: Bool, compacto :: Bool, colores :: Bool} -- Pueden agregar otros parámetros.

{-
EsNombre: Contiene a NombreM y NombreF
ConNombre: Clase con los datos que tienen nombre.
ConComentario: Clase con los datos que pueden contener comentario.
-}


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
tipoPartCodigoAgAStr (TPCCIL x) = x
tipoPartCodigoAgAStr (TPCCML x) = x

data TipoPartCodigo = TPCNomMod String | TPCImport String | TPCData String | TPCClase String | TPCInsta String | TPCFuncion String | TPCCIL String | TPCCML String deriving (Show)
data EstoyPartCod = CNormal | CNMod | CImport | CData | CClase | CInsta | CFuncion

-- Detecto Tipo de Codigo
dCod :: [TipoCodigoAg] -> [TipoPartCodigo]
dCod a = dCod' CNormal a

dCod' :: EstoyPartCod -> [TipoCodigoAg] -> [TipoPartCodigo]
dCod' _ []                                 = []
dCod' estoy ((TCAComIL x):xs)              = (TPCCIL x : dCod' estoy xs)
dCod' estoy ((TCAComML x):xs)              = (TPCCML x : dCod' estoy xs)
dCod' _ ((TCACod x):xs) | esNombreModulo x = (TPCNomMod x : dCod' CNormal xs)
dCod' _ ((TCACod x):xs) | esImport x       = (TPCImport x : dCod' CNormal xs)
dCod' _ ((TCACod x):xs) | esFuncion x      = (TPCFuncion x : dCod' CFuncion xs)
dCod' _ ((TCACod x):xs) | esData x         = (TPCData x : dCod' CData xs)
dCod' _ ((TCACod x):xs) | esClase x        = (TPCClase x : dCod' CClase xs)
dCod' _ ((TCACod x):xs) | esInstancia x    = (TPCInsta x : dCod' CInsta xs)
dCod' CFuncion ((TCACod x):xs)             = (TPCFuncion x : dCod' CFuncion xs)
dCod' CData ((TCACod x):xs)                = (TPCData x : dCod' CData xs)
dCod' CClase ((TCACod x):xs)               = (TPCClase x : dCod' CClase xs)
dCod' CInsta ((TCACod x):xs)               = (TPCInsta x : dCod' CInsta xs)

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


-- hacer que el argumento de entrada sea una lista y usar xs y xss
-- usar funcion de busqueda entre medio
-- separar esto en subfunciones

pruebaArchivoALista = quitarQuizas $ (aC <$> dCom archivoCargado)















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

dataPruebaData = Data "Estoy" "= Normal | CML | CIL | Comillas"
archivoPruebaDatas = Archivo (NombreM "") [] [dataPruebaData] [] [] []
datas :: Archivo -> [Data] -- Tipos de dato que genera
datas (Archivo _ _ d _ _ _) = d
-- para probar en GHCI : datas $ archivoPruebaDatas

clasePruebaClases = Clase (Just "(Monad m, Monad (t m))") "Transform" "t m" Nothing
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
