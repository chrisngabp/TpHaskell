module TpHaskell2Funciones where

import TpHaskell2Estructuras

-- Esto lo hicimos entre todos
{-
-
- INICIO DE FUNCIONES
-
-}

fromQuizas (OK x) = x

-- Comentarios
data Comentado a = Comentado String a

comentarioAString :: Comentario -> String
comentarioAString Nothing = ""
comentarioAString (Just x) = x

stringAComentario :: String -> Comentario
stringAComentario [] = Nothing
stringAComentario x = Just x

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

-- Hasta acá hicimos entre todos

-- Chris G.

-- Arc Vacio 
-- archivoVacio :: Archivo
-- archivoVacio = (Archivo Nothing [] [] [] [] [])


-- --AGREGAR COMENTARIOS 

-- -- Comentario Data 
-- agregarComentarioAData :: Data -> String -> Data
-- agregarComentarioAData (Data nd cod _) x = (Data nd cod (Just x))

-- -- Comentario Funcion OK
-- agregarComentarioAFuncion :: Funcion -> String -> Funcion
-- agregarComentarioAFuncion (Funcion nomb mfir pat _) x = (Funcion nomb mfir pat (Just x)) 

-- -- Comentario Instancia 
-- agregarComentarioAInstancia :: Instancia -> String -> Instancia
-- agregarComentarioAInstancia (Instancia ncla nins [] _) x = (Instancia ncla nins [] (Just x))

-- agregoComentarioAListaDeInstancias :: [Instancia] -> String -> String -> String -> [Instancia]
-- agregoComentarioAListaDeInstancias [] _ _ _ = [] 
-- agregoComentarioAListaDeInstancias ( (Instancia ncla nins ecu mcom ) : xs ) x y com | ncla == x && nins == y =  ( (Instancia ncla nins ecu (Just com)): xs )
-- agregoComentarioAListaDeInstancias ( (Instancia ncla nins ecu mcom ) : xs ) x y com =  ( (Instancia ncla nins ecu mcom ) : (agregoComentarioAListaDeInstancias xs x y com))


-- -- Comentario Clase 
-- agregoComentarioAClase :: Archivo -> String -> String -> Archivo
-- agregoComentarioAClase (Archivo nom im dat cla ins fun) x y = (Archivo nom im dat (agregarComentarioAListaDeClases c x y) ins fun)

-- agregarComentarioAListaDeClases :: [Clase] -> String -> String -> [Clase]
-- agregarComentarioAListaDeClases [] _ _ = []
-- agregarComentarioAListaDeClases ((Clase ncla tdat fun mcom):xs) x y | ncla == x = ((Clase ncla tdat fun (Just y)):xs)
-- agregarComentarioAListaDeClases ((Clase ncla tdat fun mcom):xs) x y = ((Clase ncla tdat fun mcom): (agregarComentarioAListaDeClases xs x y))


-- --QUITAR COMENTARIOS

-- eliminarComentarioAData :: Data -> Data
-- eliminarComentarioAData (Data nd cod _) = (Data nd cod Nothing)

-- eliminarComentarioAFuncion :: Funcion -> Funcion
-- eliminarComentarioAFuncion (Funcion nomb mfir pat _) = (Funcion nomb mfir pat Nothing)

-- eliminarComentarioAInstancia :: Instancia -> Instancia
-- eliminarComentarioEnInstancia (Instancia ncla nins [] _) = (Instancia ncla nins [] Nothing)

-- -- eliminoComentarioListaInstancias


-- instancias :: NombreM -> Archivo -> Quizas [Instancias]  -- Devuelve las instancias definidas de un tipo de dato
-- instancias (Archivo nm _ _ _ _ _) (Archivo instancesArc) = Maybe [Instancias]


-- nombre :: ConNombre a => a -> Nombre
-- nombre (_ nd _ _)     = nd 
-- nombre (_ nomb _ _ _) = nomb


-- nombres :: Archivo -> [Nombres]
-- nombres (Archivo (Just mnm) im dat cla ins fun) = listar mnm ++ listar im ++ listar dat ++ listar cla ++ listar ins ++ listar fun

-- listar [] = Nothing
-- listar (x:[]) = "\n" ++ show x ++ "\n"
-- listar (x:xs) = "\n" ++ show x ++ listar xs


-- getFuncion :: NombreF -> Archivo -> Quizas Funcion -- Busca una función
-- getFuncion (Funcion _ x _ _ _) (FuncionesArc[Funcion]) = Maybe [Funcion]

-- reemplazoFuncion :: NombreF -> Funcion -> Archivo -> Quizas Archivo --Reemplazo una función con una nueva
-- reemplazoFuncion (Funcion _ x _ _ _) fun = Maybe (Archivo _ _ _ _ _ [Funcion x])



-- buscoFuncion :: String -> Archivo -> [Funcion] -- Devuelve las funciones en donde se encuentra un texto
-- buscoFuncion x@(Funcion x _ _ _) (FuncionesArc:xs) = Maybe [Funcion]

-- sustituir :: String -> String -> Archivo -> Quizas Archivo -- Reemplaza y verifica que no se dupliquen nombres de funciones, datos (y parámetros ?)
-- sustituir buscoFuncion x = (Archivo _ _ _ _ _ (Funcion x _ _ _)) -- Busco la funcion y si esta la reemplazo con x ? 

-- Hasta aca hizo Chris G.

  
-- Flor
agregoDato :: Data -> Archivo -> Quizas Archivo -- Agrega un tipo de dato
agregoDato d (Archivo nm im ds c ins fa) | yaExisteDato d ds = Error "El dato ya existe"
agregoDato d (Archivo nm im ds c ins fa) = OK (Archivo nm im (d:ds) c ins fa)

yaExisteDato :: Data -> [Data] -> Bool
yaExisteDato d (d2:ds) = if dataIguales d d2 then True else yaExisteDato d ds
yaExisteDato _ _ = False

dataIguales :: Data -> Data -> Bool
dataIguales (Data nombre definicion _ ) (Data otroNombre otraDefinicion _ ) = (nombre == otroNombre) && (definicion == otraDefinicion)

sacoDato :: NombreM -> Archivo -> Archivo -- Saca un tipo de dato
sacoDato d2 (Archivo nm im d c ins fa) = Archivo nm im (sacoDato' (creoDatoSoloConNombre d2) d) c ins fa

sacoDato' _ []                 = []
sacoDato' x@(Data d1 _ _) (y@(Data d2 _ _):ys) | d1 == d2    = sacoDato' x ys
                                               | otherwise   = y : sacoDato' x ys
creoDatoSoloConNombre :: NombreM -> Data
creoDatoSoloConNombre (NombreM nombre) = Data nombre [] Nothing

agregoInstancia :: Instancia -> Archivo -> Quizas Archivo -- Agrega una instancia
agregoInstancia instancia (Archivo nm im ds c ins fa) | yaExisteInstancia instancia ins = Error "La instancia ya existe"
agregoInstancia instancia (Archivo nm im ds c ins fa) = OK (Archivo nm im ds c (instancia:ins) fa)

yaExisteInstancia :: Instancia -> [Instancia] -> Bool
yaExisteInstancia instancia (otraInstancia:inst) = if instIguales instancia otraInstancia then True else yaExisteInstancia instancia inst
yaExisteInstancia _ _ = False

instIguales :: Instancia -> Instancia -> Bool
instIguales (Instancia nombre nombreClase _ _ ) (Instancia otroNombre otroNombreClase _ _ ) = (nombre == otroNombre) && (nombreClase == otroNombreClase)

{--
agregoElemento :: E -> Archivo -> Quizas Archivo
agregoElemento elemento archivo | yaExisteElemento elemento archivo = Error "El elemento ya existe"
agregoElemento elemento (Archivo nm im ds c ins fa) | elemento is Instancia = OK (Archivo nm im ds c (elemento:ins) fa)
                                                    | elemento is Data = OK (Archivo nm im (elemento:ds) c ins fa)
--} -- TO DO ver el operador "is"

sacoInstancia :: NombreClase -> Archivo -> Archivo -- Saca una instancia de un Dato y Clase
sacoInstancia instancia (Archivo nm im d c ins fa) = Archivo nm im d c (sacoInstancia' (creoInstanciaSoloConNombre instancia) ins) fa

sacoInstancia' _ []                                                             = []
sacoInstancia' x@(Instancia d1 _ _ _) (y@(Instancia d2 _ _ _):ys) | d1 == d2    = sacoInstancia' x ys
                                                                  | otherwise   = y : sacoInstancia' x ys

creoInstanciaSoloConNombre :: NombreClase -> Instancia
creoInstanciaSoloConNombre nombre = Instancia nombre "" [] Nothing

toNombreF :: String -> NombreF -- nombre de la funcion
toNombreF nombre = NombreF nombre      -- Validar con los chicos

toNombreM :: String -> NombreM
toNombreM nombre = NombreM nombre
{--
toNombre :: EsNombre a => a -> Nombre
toNombre nombre --}

fromNombre :: Nombre -> String
fromNombre (NM nombre) = fromNombreM nombre
fromNombre (NF nombre) = fromNombreF nombre

fromNombreM :: NombreM -> String
fromNombreM (NombreM nombre) = nombre

fromNombreF :: NombreF -> String
fromNombreF (NombreF nombre) = nombre

showNombre :: Nombre -> String -- Si fuera un print, devolveria IO
showNombre nombre = fromNombre nombre

{--
comentario :: ConComentario a => a -> Comentario
comentario (ConComentario a)


type Comentario = Maybe String
class ConComentario a where
    comentario :: a -> Maybe Comentario
--}