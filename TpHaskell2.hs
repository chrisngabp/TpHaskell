-- QUE PARTE HACE CADA UNO DEL GRUPO?
-- 10% definir datos y estructuras
-- 40% lectura del texto fn leoArchivo
-- 15% Show transformar un archivo en texto
-- 35% lo demas

TP ArchivoHs

Crear un módulo ArchivoHs que realice lo siguiente:

Quizas a: Tipo de dato que puede devolver un error o un dato de tipo a
- Hacer que pertenezca a Functor, Applicative y Monad
-- Ya lo tenemos, es igual al Maybe pero tiene un texto para el Nothing

Archivo: Tipo de dato que contiene un Archivo completo
- Hacerlo pertenecer a la clase Eq y Show

data Archivo = 


Codigo: Tipo de dato que contiene los distintos bloques de cógido que puede haber.
Ej.:
data Codigo = CD Data | CF Funcion | ...

Data: Tipo de dato que contiene información sobre un dato definido.
- Hacerlo pertenecer a Eq, Ord y Show

Instancia: Tipo de dato que contiene información sobre una instancia definida.
- Hacerlo pertenecer a Eq y Show

Clase: Tipo de dato que contiene una nueva clase.
- HAcerla pertenecer a Eq, Ord y Show

Funcion: Tipo de dato que contiene la información de la función: nombre, patrones, etc.
- Hacerlo pertenecer a Eq, Ord y Show

data Funcion = Funcion NombreF (Maybe Firma) [Patron] (Maybe Comentario)
data Patron = Patron (Either Bloque [Pipe]) (Maybe Where) (Maybe Comentario)
type Firma = String -- Sinonimos (type)
type Where = [Funcion]
data Bloque = Expresion Resultado | LetIn [Funcion] Bloque | IfThenElse Bloque Bloque Bloque | Case ...
type Resultado = String
type ResultadoBool = String
data Pipe = Pipe ResultadoBool Bloque 

-- TODO: chequear que cosas tendrian Maybe Comentario

-- extrae cualquier comentario de cualquier tipo de dato
class ConComentario where
	comentario :: a -> Maybe Comentario

-- comentario miFuncion = Just "esta es mi funcion"

miArchivo :: Quizas Archivo
miArchivo = do
	a <- agregoFuncion miFuncion archivoVacio
	b <- agregoImport miImport a
	c <- agregoComentario "miFuncion" "comentario" b
	return c

miArchivo :: Quizas Archivo
miArchivo = return archivoVacio
	>>= agregoFuncion miFuncion
	>>= agregoImport miImport
	>>= agregoComentario "miFuncion" "comentario"
	>>= show




data Comentario 
-- Si la linea arranca como comentario, el comentario es de lo que sigue, y sino es inline, es de la linea


-- DUDA QUE SERIA EL PATRON??
Patron: Tipo de dato que contiene el patrón de una función, su resultado, where, etc
- Hacerlo pertenecer a Eq y Show

Bloque: expresiones, let, case .. of, if ... then .. else ..., 
- Hacerlo pertenecer a Eq y Show

-- NO SE ENTIENDE LO DE ABAJO

type Comentario = String
newtype NombreM = NombreM String -- Nombre válido para módulos, datos, etc.
newtype NombreF = NombreF String | OperadorF String -- Nombre válido para funciones
data Nombre = NM NombreM | NF NombreF

EsNombre: Contiene a NombreM y NombreF
ConNombre: Clase con los datos que tienen nombre.
ConComentario: Clase con los datos que pueden contener comentario.

data Opciones = Opciones {largoLinea :: Int, anchoTab :: Int, comentarioInline :: Bool, compacto :: Bool, colores :: Bool} -- Pueden agregar otros parámetros.

Funciones sugeridas:

defaultOpciones :: Opciones
leoArchivo :: String -> Quizas Archivo -- Lectura de un archivo completo .hs
showArchivo :: Opciones -> Archivo -> String -- Muestra el archivo con format
ordenoArchivo :: Archivo -> Archivo -- Ordeno por nombre de función

modulo :: Archivo -> Quizas NombreM -- Devuelve el módulo
imports :: Archivo -> [NombreM] -- Módulos que importa
datas :: Archivo -> [Data] -- Tipos de dato que genera
clases :: Archivo -> [Clase] -- Clases que genera
funciones :: Archivo -> [Funciones] -- Funciones definidas
instancias :: NombreM -> Archivo -> Quizas [Instancias] -- Devuelve las instancias definidas de un tipo de dato
nombres :: Archivo -> [Nombre] -- Lista de clases, datos, funciones, etc incluidas (solo de primer nivel).

agregoFuncion :: Funcion -> Archivo -> Quizas Archivo -- Agrega una función
-- tiene un Quizas archivo porque si ya existe la funcion devuelvo un error
sacoFuncion :: NombreF -> Archivo -> Archivo -- Devuelve el Archivo sacando una función
reemplazoFuncion :: NombreF -> Funcion -> Archivo -> Quizas Archivo -- Reemplazo una función con una nueva
buscoFuncion :: String -> Archivo -> [Funcion] -- Devuelve las funciones en donde se encuentra un texto
agregoDato :: Data -> Archivo -> Quizas Archivo -- Agrega un tipo de dato
sacoDato :: NombreM -> Archivo -> Archivo -- Saca un tipo de dato
agregoInstancia :: Instancia -> Archivo -> Quizas Archivo -- Agrega una instancia
sacoInstancia :: NombreM -> NombreM -> Archivo -> Archivo -- Saca una instancia de un Dato y Clase
agregoComentario -> Comentario -> Nombre -> Archivo -> Quizas Archivo -- Agrego un comentario a determinada Función, Data, etc. Si ya contiene uno, lo agrega al comentario anterior.
sacoComentario -> Nombre -> Archivo -> Archivo -- Saco el comentario
getFuncion :: NombreF -> Archivo -> Quizas Funcion -- Busca una función
archivoVacio :: Archivo -- Archivo Vacío
sustituir :: String -> String -> Archivo -> Quizas Archivo -- Reemplaza y verifica que no se dupliquen nombres de funciones, datos (y parámetros ?)
nombre :: ConNombre a => a -> Nombre -- Devuelve el nombre de un tipo de dato (ej. el nombre de una función o un tipo de dato)
toNombreF :: String -> NombreF -- nombre de la funcion
toNombreM :: String -> NombreM
toNombre :: EsNombre a => a -> Nombre
fromNombre :: Nombre -> String
showNombre :: Nombre -> String
comentario :: ConComentario a => a -> Comentario

