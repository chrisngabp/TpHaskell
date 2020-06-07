module TpHaskell2Index where

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

data Archivo = Archivo NombreM [Importacion] [Data] [Clase] [Instancia] [Funcion]

instance Show Archivo where
    show = undefined -- mostrarArchivo

-- instance Eq Archivo where

data Importacion = Importacion NombreM Comentario deriving (Show) -- "Data.List" "import Data.List (inits)"

data Codigo = CD Data | CC Clase | CI Instancia | CF Funcion

data Data = Data NombreDeDato String deriving (Show)
type NombreDeDato = String

data Clase = Clase (Maybe HerenciaClase) NombreClase Firma (Maybe Where) deriving (Show)
type NombreClase = String
type HerenciaClase = String 

data Instancia = Instancia NombreClase NombreDeDato Where
type TipoInstancia = String

-- INICIO Datas y types de Funcion

data Funcion = Funcion NombreF (Maybe Firma) [Patron] Comentario deriving (Show)
type Firma = String -- Sinonimos (type)

data Patron = Patron (Either Bloque [Pipe]) (Maybe Where) Comentario deriving (Show)
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
data NombreF = NombreF String | OperadorF String deriving (Show) -- Nombre válido para funciones
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

-- detecto comentario
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

-- agrupo comentario
data TipoCodigoAg = TCACod String | TCAComIL String | TCAComML String deriving (Show)

aC :: [TipoCodigo] -> [TipoCodigoAg]
aC []               = []
aC xs'@(TCCod x:xs) = map TCACod linIni ++ aC fin
    where (ini,fin) = break (not . esCodigo) xs'
          ini'      = map tipoCodigoAChr ini
          linIni    = filter (not . null) $ lineas ini'
aC xs'@(TCComIL x:xs) = TCAComIL (map tipoCodigoAChr ini) : aC fin
    where (ini,fin) = break esCodigo xs'
aC xs'@(TCComML x:xs) = TCAComML (map tipoCodigoAChr ini) : aC fin
    where (ini,fin) = break esCodigo xs'

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

-- Para probar esto -> agrupoComentario <$> dCom "string con cosas"

archivoPruebaModulo = Archivo (NombreM "TpHaskell2Index") [] [] [] [] []
modulo :: Archivo -> Quizas NombreM -- Devuelve el módulo
modulo (Archivo nm _ _ _ _ _) = OK nm

importPruebaImports = Importacion (NombreM "Data.List (inits)") (Just "prueba") --"-- agrego lista"
importPruebaImports2 = Importacion (NombreM "Data.List") Nothing
archivoPruebaImports = Archivo (NombreM "") [importPruebaImports, importPruebaImports2] [] [] [] []
imports :: Archivo -> [Importacion] -- Módulos que importa
imports (Archivo _ im _ _ _ _) = im

dataPruebaData = Data "Estoy" "= Normal | CML | CIL | Comillas"
archivoPruebaDatas = Archivo (NombreM "") [] [dataPruebaData] [] [] []
datas :: Archivo -> [Data] -- Tipos de dato que genera
datas (Archivo _ _ d _ _ _) = d

clasePruebaClases = Clase (Just "(Monad m, Monad (t m))") "Transform" "t m" Nothing
archivoPruebaClases = Archivo (NombreM "") [] [] [clasePruebaClases] [] []
clases :: Archivo -> [Clase] -- Clases que genera
clases (Archivo _ _ _ c _ _) = c

{-
funciones :: Archivo -> [Funciones] -- Funciones definidas
instancias :: NombreM -> Archivo -> Quizas [Instancias] -- Devuelve las instancias definidas de un tipo de dato
nombres :: Archivo -> [Nombre] -- Lista de clases, datos, funciones, etc incluidas (solo de primer nivel).
-}