data Funcion = Funcion {nombreF :: NombreF, firmaF :: Maybe Firma, patronesF :: [Patron], comentarioF :: Comentario}
-- podria usar comentarioDeLaFuncion = comentarioF $ funcionnueva

instance Show Funcion where
    show = showFuncion -- hay que tener en cuenta las "opciones" de salida

showFuncion :: Funcion -> String
showFuncion f@(Funcion {comentarioF = Just c}) = intercalate "\n" (map ("-- " ++) (lineas c)) ++ "\n" ++ showFuncionFirma f
showFuncion f@(Funcion {comentarioF = Nothing}) = showFuncionFirma f

-- import LoadFile
-- import Data.List para usar intercalate

showFuncionFirma f@(Funcion {firmaF = Nothing}) = showFuncionPatron f
showFuncionFirma f@(Funcion {nombreF = n, firmaF = Just miFirma}) = n ++ " :: " ++ miFirma ++ "\n" ++ showFuncionPatron f

showFuncionPatron f@(Funcion {patronesF = []}) = ""
showFuncionPatron f@(Funcion {nombreF = nf, patronesF = (p:ps)} = showFuncionPatron' nf p ++ "\n" ++ showFuncionPatron (f {patronesF = ps}) -- esto cambia el campo patrones a f y la mandamos asi

showFuncionPatron' :: NombreF -> Patron -> String
showFuncionPatron' nf p@(Patron {comentarioPr = Nothing}) = showFuncionPatronArg nf p
-- falta el Just

showFuncionPatronArg nf p@(Patron {argumentosPt = a}) = nf ++ " " ++ a ++ showFuncionPatronPipes p

showFuncionPatronPatron p(Patron {patron = Left (Expresion bloque com)}) = " = " ++ bloque ++ showFuncionPatronWhere p
showFuncionPatronPatron p(Patron {patron = Right []}) = "\n"
showFuncionPatronPatron p(Patron {patron = Right (Pipe condix (Expresion bloque _):pipes)}) = "\n   | "  ++ condix ++ " = " ++ bloque ++ "\n" ++ showFuncionPatronPatron (p {patron = Right pipes})

-- el tabulado sacarlo de una funcion, no dejarlo asi

-- falta mostrar el comentario

showFuncionPatronWhere _ = ""
-- completar esto






