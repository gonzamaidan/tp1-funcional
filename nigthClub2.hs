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
							
