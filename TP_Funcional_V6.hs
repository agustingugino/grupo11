module TP_Funcional_V6 where
import Text.Show.Functions
type Desgaste = Float
type Patente = String
type Fecha = (Int, Int, Int)

-- Definiciones base
anio :: Fecha -> Int
anio (_, _, year) = year


data Auto = Auto {
 patente :: Patente,
 desgasteLlantas :: [Desgaste],
 rpm :: Integer,
 temperaturaAgua :: Float,
 ultimoArreglo :: Fecha
} deriving (Show)


----------------------------------------------PARA TESTEAR ---------------------------------------------------------------
autoA = Auto {patente = "DJB123", desgasteLlantas = [0.1, 0.2, 0.1, 0.3], rpm= 1500, temperaturaAgua = 55.55, ultimoArreglo = (01,01,2019)}
autoB = Auto {patente = "NBB123", desgasteLlantas = [0.2, 0.5 ,0.6 ,0.1], rpm= 3000, temperaturaAgua = 55.55, ultimoArreglo = (01,01,2000) }
autoC = Auto {patente = "DJB124", desgasteLlantas = [1,2,1,3], rpm= 1500, temperaturaAgua = 55.55, ultimoArreglo = (01,01,2020) }
autoD = Auto {patente = "NBB124", desgasteLlantas = [1,2,1,3], rpm= 1500, temperaturaAgua = 55.55, ultimoArreglo = (01,01,2020) }
autoE = Auto {patente = "ACB123", desgasteLlantas = [1,2,1,3], rpm= 1500, temperaturaAgua = 55.55, ultimoArreglo = (01,01,2004) }
autoF = Auto {patente = "XXB123", desgasteLlantas = [1,2,1,3], rpm= 1500, temperaturaAgua = 55.55, ultimoArreglo = (01,01,2020) }
autoG = Auto {patente = "EFB123", desgasteLlantas = [1,2,1,3], rpm= 1500, temperaturaAgua = 55.55, ultimoArreglo = (01,01,2015) }
autoH = Auto {patente = "ZZB124", desgasteLlantas = [1,2,1,3], rpm= 1500, temperaturaAgua = 55.55, ultimoArreglo = (01,01,2020) }

type Flota = [Auto]
flotaDeAutos :: Flota
flotaDeAutos = [autoA,autoB,autoC,autoD,autoE,autoF,autoG,autoH]
flotaDeAutosPares = [autoA,autoB]

--Esta lista de autos: un auto con desgaste de cubiertas [0.1, 0.4, 0.2, 0], otro auto con desgaste [0.3, 0.5, 0.6, 0.1], y otro con desgaste [0.1, 0.1, 0.1, 0]
autoI = Auto {patente = "XXB123", desgasteLlantas = [0.1, 0.4, 0.2, 0.1], rpm= 1500, temperaturaAgua = 55.55, ultimoArreglo = (01,01,2020) }
autoJ = Auto {patente = "EFB123", desgasteLlantas = [0.3, 0.5, 0.6, 0.1], rpm= 1500, temperaturaAgua = 55.55, ultimoArreglo = (01,01,2020) }
autoK = Auto {patente = "ZZB124", desgasteLlantas = [0.1, 0.1, 0.1, 0], rpm= 1500, temperaturaAgua = 55.55, ultimoArreglo = (01,01,2020) }

-----------------------------------------------------------------------------------------------------
--
cambiarPatente nuevaPatente auto = Auto {
 patente = nuevaPatente ,
 desgasteLlantas = desgasteLlantas auto,
 rpm = rpm auto,
 temperaturaAgua = temperaturaAgua auto,
 ultimoArreglo = ultimoArreglo auto 
} 

cambiarRpm nuevoRpm auto = Auto {
 patente = patente auto,
 desgasteLlantas = desgasteLlantas auto,
 rpm = nuevoRpm ,
 temperaturaAgua = temperaturaAgua auto,
 ultimoArreglo = ultimoArreglo auto 
} 

--- Armo una primera lista con "0.5" de la cantidad de cubiertas que quiero, luego concateno las que restan. Si no resta ninguna la lista que concatena es la vacia.

cambiarCubiertasASinDesgaste (x:xs) cantidadCubiertas = [ 0.5 | x <- (take cantidadCubiertas (x:xs) ), x >  0] ++ (drop cantidadCubiertas (x:xs))

cambiarCubiertas cantidadCubiertas auto = Auto {
 patente = patente auto,
 desgasteLlantas = cambiarCubiertasASinDesgaste (desgasteLlantas auto) cantidadCubiertas,
 rpm = rpm auto ,
 temperaturaAgua = temperaturaAgua auto,
 ultimoArreglo = ultimoArreglo auto 
} 

cambiarTemperaturaANoventa auto = Auto {
 patente = patente auto,
 desgasteLlantas = desgasteLlantas auto,
 rpm = rpm auto ,
 temperaturaAgua = 90,
 ultimoArreglo = ultimoArreglo auto 
} 

cambiarFechaDeReparacion fecha auto = Auto {
 patente = patente auto,
 desgasteLlantas = desgasteLlantas auto,
 rpm = rpm auto ,
 temperaturaAgua = temperaturaAgua auto,
 ultimoArreglo = fecha 
} 


------------------------------------------------------------------------------------------------------------
--PUNTO 1 
------
--Obs: Quizas lo pueda resolver por recursividad con alguna funcion que tome 2 letras y te diga si esta entre esas...

entreLetrasDJNB :: String -> Bool
entreLetrasDJNB patente = "DJ" <= (take 2 patente) && (take 2 patente) <= "NB"

ultimoCaracterPatente = last

calculoPatental :: String -> Integer
calculoPatental patente | (entreLetrasDJNB patente == True) && (ultimoCaracterPatente patente =='4') = (*) 3000 6
                        | (entreLetrasDJNB patente == True) && (ultimoCaracterPatente patente /= '4') = 20000
                        | otherwise = 15000

costoReparacionAuto :: Auto -> Integer
esPatenteNueva :: String -> Bool

esPatenteNueva = (==7).length
costoReparacionAuto auto | (esPatenteNueva.patente) auto = 12500
                         | otherwise = (calculoPatental.patente) auto 

------------------------------------------------------------------------------------------------------------
--PUNTO 2 

autoPeligroso :: Auto -> Bool
exedioDesgasteLimiteDeLlantas :: [Float] -> Bool

exedioDesgasteLimiteDeLlantas = (>0.5).head
autoPeligroso auto = (exedioDesgasteLimiteDeLlantas.desgasteLlantas) auto

necesitaRevision :: Auto ->  Bool
exedioAnioDeReparacion :: Fecha -> Bool

exedioAnioDeReparacion = (<=2015).anio
necesitaRevision auto = (exedioAnioDeReparacion.ultimoArreglo) auto

------------------------------------------------------------------------------------------------------------
--PUNTO 3

type Mecanico = (Auto -> Auto)

tecnicoAlfa :: Mecanico
tecnicoAlfa auto | (((>=2000).rpm) auto) = (cambiarRpm 2000 auto)
  | otherwise =  auto

tecnicoBravo :: Mecanico 
tecnicoBravo auto = cambiarCubiertas 4 auto

tecnicoCharlie :: Mecanico
tecnicoCharlie = tecnicoAlfa.tecnicoBravo

tecnicoTango :: Mecanico
tecnicoTango auto = auto

tecnicoLima :: Mecanico
tecnicoLima auto = cambiarCubiertas 2 auto

tecnicoZulu :: Mecanico
tecnicoZulu = cambiarTemperaturaANoventa.tecnicoLima

tecnicos :: [Mecanico] 
tecnicos =  [tecnicoAlfa,tecnicoBravo,tecnicoCharlie,tecnicoTango,tecnicoLima,tecnicoZulu]

------------------------------------------------------------------------------------------------------------
--PUNTO 4

sumarLista = sum
cantidadDeDesgaste :: [Float] -> Int
cantidadDeDesgaste = round . (*10) . sumarLista
 
armoListaDeDesgastes :: [Auto] -> [Int]
armoListaDeDesgastes (x:xs) = [cantidadDeDesgaste (desgasteLlantas x) | x <- (x:xs)] 

listasDeIndicesDeUnaLista (x:xs) = [[indiceGenerado,x] | (x,indiceGenerado) <- zip (x:xs) [1..length (x:xs)]]  

armoListaDeDesgastesConIndice = listasDeIndicesDeUnaLista . armoListaDeDesgastes



--Preguntar a Edu como se puede utilizar composicion con el ALL

laListaCumpleConLaFuncion :: (a->Bool)->[a]->Bool
laListaCumpleConLaFuncion funcion = all (funcion) 

evaluoOrdenamientoToc :: (Integral a) => [[a]]->Bool
evaluoOrdenamientoToc [] = True
evaluoOrdenamientoToc (x:xs) | (((laListaCumpleConLaFuncion odd x) || (laListaCumpleConLaFuncion even x)) == True) =  evaluoOrdenamientoToc xs
                            | otherwise = False

ordenamientoToc :: [Auto]->Bool
ordenamientoToc = evaluoOrdenamientoToc.armoListaDeDesgastesConIndice

--------------------------------------------------------------------------------
--PUNTO 5
--Orden de reparación
--y consiste en que cada uno de los técnicos realice las reparaciones que sabe sobre el auto, al que además hay que actualizarle la última fecha de reparación.
--tecnicoBravo,tecnicoCharlie,tecnicoTango,tecnicoLima

ejecutarTecnico :: Auto ->(Auto->Auto)->Auto

ejecutarTecnico unAuto unTecnico = unTecnico unAuto

foldeoDeTecnicos auto lsDeTecnicos = (foldl (ejecutarTecnico) (auto) (lsDeTecnicos))

ordenDeReparacion lsDeTecnicos fecha auto = cambiarFechaDeReparacion fecha (foldeoDeTecnicos auto lsDeTecnicos)

------------------------------------------------------------------------------------------------
--PUNTO 6

tecnicosQueDejanElAutoEnCondiciones :: [a->Auto]->a->[a->Auto]
tecnicosQueDejanElAutoEnCondiciones listaTecnicos auto = filter (not.autoPeligroso.(flip id auto)) (listaTecnicos)


--Parte 2) Integrante b: Costo de reparación de autos que necesitan revisión

--Dada una lista de autos, saber cuál es el costo de reparación de los autos que necesitan revisión.

autosQueNecesitanReparacion :: [Auto] -> [Auto]
autosQueNecesitanReparacion = filter (necesitaRevision) 

listaDeCostoDeReparacionDeAutosQueNecesitanRevision :: [Auto] -> [Integer]
listaDeCostoDeReparacionDeAutosQueNecesitanRevision listaDeAutos = map costoReparacionAuto (autosQueNecesitanReparacion listaDeAutos)

costoDeReparacionDeAutosQueNecesitanRevision :: [Auto] -> Integer
costoDeReparacionDeAutosQueNecesitanRevision listaDeAutos = foldl (+) 0 (listaDeCostoDeReparacionDeAutosQueNecesitanRevision listaDeAutos)
--------------------------------------------------------------------------------
--PUNTO 8
--Punto A)

--ff auto lista = not (autoPeligroso (lista auto))
--primerTecnicoQueDejaElAutoEnCondiciones [] auto = []
--primerTecnicoQueDejaElAutoEnCondiciones (x:xs) auto | ((ff auto x) == True) = x                                                   
--                                                    | otherwise = primerTecnicoQueDejaElAutoEnCondiciones xs auto

tecnicosInfinitos = tecnicoTango:tecnicosInfinitos


primerTecnicoQueDejaElAutoEnCondiciones :: [Mecanico]->Auto->[Mecanico]
primerTecnicoQueDejaElAutoEnCondiciones listaDeTecnicos auto = take 1 (tecnicosQueDejanElAutoEnCondiciones listaDeTecnicos auto)

{-
Como Haskell funciona con Lazy Evaluation, al encontrar el primer elemento que cumpla con la condición impuesta, 
por mas que sea una lista infinita, nos retornara el  valor obtenido. En el caso, en el que la lista infinita es de "tecnicoTango" , 
la función no se puede ejecutar ya que nunca va a hallar un técnico que deje el auto en condiciones.

En este caso, cuando la funcion "tecnicosQueDejanElAutoEnCondiciones" genere un elemento de la lista (osea un tecnico que deja el auto en condiciones)
el "take 1" gracias al Lazy Evaluation devolvera este Tecnico.
-}

----------

--Punto B)

autosInfinitosA :: [Auto]
autosInfinitosA = autosInfinitosAA 0 0
autosInfinitosAA :: Integer-> Float -> [Auto]
autosInfinitosAA n flo = Auto {
patente = "AAA000",
desgasteLlantas = [flo, 0, 0, 0.3],
rpm = 1500 + n,
temperaturaAgua = 90,
ultimoArreglo = (20, 1, 2013)
} : autosInfinitosAA (n + 1) (flo + 1)

autosInfinitosB :: [Auto]
autosInfinitosB = autosInfinitosBB 0 0
autosInfinitosBB :: Integer-> Float -> [Auto]
autosInfinitosBB n flo = Auto {
patente = "DJA000",
desgasteLlantas = [flo, 0, 0, 0.3],
rpm = 1500 + n,
temperaturaAgua = 90,
ultimoArreglo = (20, 1, 2013)
} : autosInfinitosBB (n + 1) (flo + 1)

autosInfinitosC :: [Auto]
autosInfinitosC = autosInfinitosCC 0 0
autosInfinitosCC :: Integer-> Float -> [Auto]
autosInfinitosCC n flo = Auto {
patente = "DJA004",
desgasteLlantas = [flo, 0, 0, 0.3],
rpm = 1500 + n,
temperaturaAgua = 90,
ultimoArreglo = (20, 1, 2013)
} : autosInfinitosCC (n + 1) (flo + 1)

autosInfinitosD :: [Auto]
autosInfinitosD = autosInfinitosDD 0 0
autosInfinitosDD :: Integer-> Float -> [Auto]
autosInfinitosDD n flo = Auto {
patente = "AA000AA",
desgasteLlantas = [flo, 0, 0, 0.3],
rpm = 1500 + n,
temperaturaAgua = 90,
ultimoArreglo = (20, 1, 2013)
} : autosInfinitosDD (n + 1) (flo + 1)

--En base al punto “Dada una lista de autos, saber cuál es el costo de reparación de los autos que necesitan revisión.”, ¿podríamos tener una lista infinita de autos?
-- No es posible tener una lista infinita de autos, debido a que para poder sumar los costos de reparacion, la lista debe ser finita, sino nunca terminaria de evaluar
-- los autos que necesiten reparacion.


costoDeReparacionDeTresPrimerosAutosQueNecesitanRevision :: [Auto] -> Integer
costoDeReparacionDeTresPrimerosAutosQueNecesitanRevision listaDeAutos = foldl (+) 0 (take 3 (listaDeCostoDeReparacionDeAutosQueNecesitanRevision listaDeAutos))

{-
Y si tomáramos en cuenta los tres primeros autos que necesitan revisión, ¿cómo
debería cambiar la función? Por otra parte, ¿esta versión aceptaría una lista infinita de autos?
Modifique la función 6.b con otro nombre y justifique sus respuestas.

La funcion queda definida como en "costoDeReparacionDeTresPrimerosAutosQueNecesitanRevision", esta version acepta una lista infinita de autos, debido a lo mismo
planteado en el punto anterior, por el Lazy Evaluation de Haskell.

En la funcion de ejemplo, el "take 3" creara una lista al encontrar los 3 primeros elementos que cumplan de la lista infinita. Luego el fold sumara estos 3 elementos.
-}