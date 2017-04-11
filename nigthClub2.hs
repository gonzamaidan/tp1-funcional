module TP where

data Cliente = Cliente {
						nombre:: String,
						resistencia:: Int,
						amigos:: [Cliente]
						} deriving (Show, Eq)
						
rodri =  Cliente "Rodri" 55 []
marcos = Cliente "Marcos" 40 [rodri]
cristian = Cliente "Cristian" 2 []
ana = Cliente "Ana" 120 [marcos, rodri]

comoEsta :: Cliente -> String
comoEsta cliente	| (>50) (resistencia cliente) = "fresco"
					| (>1) (length (amigos cliente)) = "piola"
					| otherwise = "duro"
					
					
sonMismoCliente :: Cliente -> Cliente -> Bool
sonMismoCliente cliente otroCliente = (nombre cliente) == (nombre otroCliente)

noEsAmigo :: Cliente -> Cliente -> Bool
noEsAmigo cliente otroCliente = null (filter (sonMismoCliente otroCliente) (amigos cliente))


agregaAmigo cliente amigo = cliente { amigos = (amigo:(amigos cliente))}

agregarAmigo cliente amigo	| sonMismoCliente cliente amigo = cliente
							| noEsAmigo cliente amigo = (agregaAmigo cliente amigo)
							| otherwise = cliente

							

type Bebida = Cliente -> Cliente
type CambioResistencia = Int -> Cliente -> Cliente

cambiarResistencia :: CambioResistencia
aumentarResistencia :: CambioResistencia
bajarResistencia :: CambioResistencia

cambiarResistencia nuevaResistencia cliente = cliente { resistencia = nuevaResistencia }
aumentarResistencia nuevaResistencia cliente= cambiarResistencia (resistencia cliente + nuevaResistencia) cliente
bajarResistencia nuevaResistencia cliente = cambiarResistencia (max 0 (resistencia cliente - nuevaResistencia)) cliente
bajarResistenciaConAmigos nuevaResistencia cliente = cliente { resistencia = resistencia cliente - nuevaResistencia, amigos = map (bajarResistencia nuevaResistencia) (amigos cliente)}

grog :: Bebida
grog = cambiarResistencia 0

jarraLoca :: Bebida
jarraLoca cliente = (bajarResistenciaConAmigos 10 cliente)

klusner:: String -> Bebida 
klusner gusto cliente = bajarResistencia (length gusto) cliente

cantidadAmigos = length.amigos

tintico :: Bebida
tintico cliente = aumentarResistencia (((*5).cantidadAmigos) cliente) cliente

agregarANombre texto cliente = cliente { nombre = texto ++ (nombre cliente)} 

soda :: Int -> Bebida
soda fuerza = agregarANombre ("e" ++ (concat.replicate fuerza) "r" ++ "p") 


rescatarse :: Int -> Cliente -> Cliente
rescatarse horas 	| ( horas > 3 ) = aumentarResistencia 200
					| ( horas > 0 ) = aumentarResistencia 100

					
comoEstaDespuesTomar:: Bebida -> Cliente -> String 
comoEstaDespuesTomar bebida = (comoEsta.bebida)

