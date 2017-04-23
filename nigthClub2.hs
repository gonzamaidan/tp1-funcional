module TP where
import Text.Show.Functions
import Data.List

data Cliente = Cliente {
						nombre:: String,
						resistencia:: Int,
						amigos:: [Cliente],
						bebidas:: [Bebida]
						} deriving (Show)
						
rodri =  Cliente "Rodri" 55 [] [tintico]
marcos = Cliente "Marcos" 40 [rodri] [klusener "guinda"]
cristian = Cliente "Cristian" 2 [] [grogXD,jarraLoca]
ana = Cliente "Ana" 120 [marcos, rodri] []
robertoCarlos = Cliente "Roberto Carlos" 165 [] []
chuckNorris = Cliente "Chuck Norris" 1000 [ana] (sodasDeChuck 1)

sodasDeChuck :: Int -> [Bebida]
sodasDeChuck fuerza = (soda fuerza):(sodasDeChuck (fuerza+1))


comoEsta :: Cliente -> String
comoEsta cliente	| ((>50).resistencia) cliente = "fresco"
					| ((>1).length.amigos) cliente = "piola"
					| otherwise = "duro"
					
					
sonMismoCliente :: Cliente -> Cliente -> Bool
sonMismoCliente cliente otroCliente = (nombre cliente) == (nombre otroCliente)

noEsAmigo :: Cliente -> Cliente -> Bool
noEsAmigo cliente otroCliente = null (filter (sonMismoCliente otroCliente) (amigos cliente))


agregaAmigo cliente amigo = cliente { amigos = (amigo:(amigos cliente))}

hacerseAmigoDe cliente amigo	| sonMismoCliente cliente amigo = cliente
							| noEsAmigo cliente amigo = (agregaAmigo cliente amigo)
							| otherwise = cliente

agregarMuchosAmigos :: Cliente -> [Cliente] -> Cliente
agregarMuchosAmigos cliente = foldl hacerseAmigoDe cliente 

type Bebida = Cliente -> Cliente
type CambioResistencia = Int -> Cliente -> Cliente

cambiarResistencia :: CambioResistencia
aumentarResistencia :: CambioResistencia
bajarResistencia :: CambioResistencia

cambiarResistencia nuevaResistencia cliente = cliente { resistencia = nuevaResistencia }
aumentarResistencia nuevaResistencia cliente= cambiarResistencia (resistencia cliente + nuevaResistencia) cliente
bajarResistencia nuevaResistencia cliente = cambiarResistencia (max 0 (resistencia cliente - nuevaResistencia)) cliente
bajarResistenciaConAmigos nuevaResistencia cliente = cliente { resistencia = resistencia cliente - nuevaResistencia, amigos = map (bajarResistencia nuevaResistencia) (amigos cliente)}

agregarBebida bebida cliente = cliente { bebidas  = bebida : bebidas cliente }

grogXD :: Bebida
grogXD = ((agregarBebida grogXD).(cambiarResistencia 0))

jarraLoca :: Bebida
jarraLoca cliente = ((agregarBebida jarraLoca).(bajarResistenciaConAmigos 10)) cliente

klusener:: String -> Bebida 
klusener gusto cliente = ((agregarBebida (klusener gusto)).(bajarResistencia (length gusto))) cliente

cantidadAmigos = length.amigos

tintico :: Bebida
tintico cliente = ((agregarBebida tintico).(aumentarResistencia (((*5).cantidadAmigos) cliente))) cliente

agregarANombre texto cliente = cliente { nombre = texto ++ (nombre cliente)} 

soda :: Int -> Bebida
soda fuerza = ((agregarBebida (soda fuerza)).(agregarANombre ("e" ++ (concat.replicate fuerza) "r" ++ "p"))) 


jarraPopular :: Int -> Bebida
jarraPopular 0 cliente = cliente
jarraPopular espirituosidad cliente = jarraPopular (espirituosidad-1) (agregarMuchosAmigos cliente (iterate (concat.(map amigos)) (amigos cliente) !! espirituosidad))

rescatarse :: Int -> Cliente -> Cliente
rescatarse horas 	| ( horas > 3 ) = aumentarResistencia 200
					| ( horas > 0 ) = aumentarResistencia 100

					
comoEstaDespuesTomar:: Bebida -> Cliente -> String 
comoEstaDespuesTomar bebida = (comoEsta.bebida)

tomarTragos ::  Cliente -> [Bebida] -> Cliente
tomarTragos cliente [] = cliente
tomarTragos cliente (trago:tragos) = tomarTragos (trago cliente) tragos

realizarAcciones ::  Cliente -> [(Cliente -> Cliente)] -> Cliente
realizarAcciones cliente [] = cliente
realizarAcciones cliente (accion:acciones) = realizarAcciones (accion cliente) acciones


dameOtro :: Cliente -> Cliente
dameOtro cliente = ((head.bebidas) cliente) cliente

cualesPuedeTomar :: Cliente -> [Bebida] -> [Bebida]
cualesPuedeTomar cliente tragos = filter ((>0).resistencia.($ cliente)) tragos

cuantasPuedeTomar:: Cliente -> [Bebida] -> Int
cuantasPuedeTomar cliente = (length.(cualesPuedeTomar cliente))

data Itinerario = Itinerario {
								nombreItinerario:: String,
								duracionEstimada::Float,
								acciones::[Cliente -> Cliente]
							} deriving (Show)

				
mezclaExplosiva = Itinerario "Mezcla explosiva" 2.5 [grogXD, grogXD, klusener "huevo", klusener "frutilla"]
itinerarioBasico = Itinerario "Itinerario basico" 5.0 [jarraLoca, klusener "chocolate", rescatarse 2, klusener "huevo"]
salidaDeAmigos = Itinerario "Salida de amigos" 1.0 [soda 1, tintico, flip hacerseAmigoDe robertoCarlos, jarraLoca]

hacerItinerario ::  Cliente -> Itinerario -> Cliente
hacerItinerario cliente itinerario = realizarAcciones cliente (acciones itinerario)

intensidad :: Itinerario -> Float
intensidad itinerario = ((genericLength.acciones) itinerario) / (duracionEstimada itinerario)

itinerarioMasIntenso :: [Itinerario] -> Itinerario
itinerarioMasIntenso = foldl1 (\itinerario1 itinerario2 -> if(intensidad itinerario1 > intensidad itinerario2) then itinerario1 else itinerario2)

hacerItinerarioMasIntenso :: Cliente -> [Itinerario] -> Cliente
hacerItinerarioMasIntenso cliente itinerarios = hacerItinerario cliente (itinerarioMasIntenso itinerarios)

tomarBebida bebida cliente = bebida cliente 