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