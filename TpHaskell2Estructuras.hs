module TpHaskell2Estructuras where

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

data Data = Data {nombreDat :: NombreDeDato, definicionDat :: String, comentarioDat :: Comentario} deriving (Show)
type NombreDeDato = String

data Clase = Clase {herenciaCla :: Maybe HerenciaClase, nombreCla :: NombreClase, firmaCla :: Firma, whereCla :: Where, comentarioCla :: Comentario} deriving (Show)
type NombreClase = String
type HerenciaClase = String 

data Instancia = Instancia {nombreIns :: NombreClase, nombreDatoIns :: NombreDeDato, whereIns :: Where, comentarioIns :: Comentario} deriving (Show)
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

