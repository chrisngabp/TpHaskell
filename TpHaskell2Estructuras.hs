data Quizas a = Error String | OK a deriving (Show)

instance Functor Quizas where
    fmap _ (Error x) = Error x
    fmap f (OK x) = OK $ f x

instance Applicative Quizas where
    pure x			= OK x
    _ <*> Error x 	= Error x
    Error x <*> _	= Error x
    OK f <*> OK x	= OK $ f x

instance Monad Quizas where
    Error x >>= _ = Error x
    OK x >>= f 	  = f x

data Archivo = Archivo NombreM [Importacion] [Data] [Clase] [Instancia] [Funcion]

instance Show Archivo where
    show = undefined -- mostrarArchivo

-- instance Eq Archivo where

data Importacion = Importacion NombreM String (Maybe Comentario) -- "Data.List" "import Data.List (inits)"

data Codigo = CD Data | CC Clase | CI Instancia | CF Funcion

data Data = Data NombreDeDato String
type NombreDeDato = String

data Clase = Clase (Maybe HerenciaClase) NombreClase Firma (Maybe Where)
type NombreClase = String
type HerenciaClase = String 

data Instancia = Instancia NombreClase NombreDeDato Where
type TipoInstancia = String

-- INICIO Datas y types de Funcion

data Funcion = Funcion NombreF (Maybe Firma) [Patron] (Maybe Comentario)
type Firma = String -- Sinonimos (type)

data Patron = Patron (Either Bloque [Pipe]) (Maybe Where) (Maybe Comentario)
type Where = [Funcion]
data Bloque = 	Expresion Resultado | 
                LetIn [Funcion] Bloque | 
                IfThenElse Bloque Bloque Bloque | 
                Case [Bloque]

type Resultado = String
type ResultadoBool = String
data Pipe = Pipe ResultadoBool Bloque 

-- FIN Datas y types de Funcion

type Comentario = String
class ConComentario a where
    comentario :: a -> Maybe Comentario

data NombreM = NombreM String -- Nombre válido para módulos, datos, etc.
data NombreF = NombreF String | OperadorF String -- Nombre válido para funciones
data Nombre = NM NombreM | NF NombreF

data Opciones = Opciones {largoLinea :: Int, anchoTab :: Int, comentarioInline :: Bool, compacto :: Bool, colores :: Bool} -- Pueden agregar otros parámetros.

{-
EsNombre: Contiene a NombreM y NombreF
ConNombre: Clase con los datos que tienen nombre.
ConComentario: Clase con los datos que pueden contener comentario.
-}


