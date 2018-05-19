type Condicion = Viaje -> Bool

data Chofer = Chofer { nombre :: String, kilometraje :: Int, viajesTomados :: [Viaje], condicion :: Condicion }
data Viaje = Viaje { fecha :: (Int, Int, Int), cliente :: Cliente, costo :: Int }
data Cliente = Cliente { nombre :: String, direccion :: String }
-- el chofer del viaje son independientes uno de otro. No asi viaje de cliente (van juntos siempre)

cualquierViaje :: Condicion
cualquierViaje _ = True

viajesMayorA200Pesos :: Condicion
viajesMayorA200Pesos = (>200).costo

clienteNLetras :: Int -> Condicion
clienteNLetras cantidadLetras = (>cantidadLetras).length.nombre.cliente

clienteNoViveEn :: String -> Condicion
clienteNoViveEn zona = (/=zona).direccion.cliente

lucas = Cliente "Lucas" "Victoria"
daniel = Chofer "Daniel" 23500 [ Viaje (20,04,2017) , lucas , 150 ] (clienteNoViveEn "Olivos")
alejandra = Chofer "Alejandra" 180000 [] cualquierViaje

choferPuedeTomarElViaje :: Viaje -> Chofer -> Bool
choferPuedeTomarElViaje unViaje unChofer = condicion unChofer $ unViaje

liquidacionDelChofer :: Chofer -> Int
liquidacionDelChofer =  sumarTodosLosViajes.viajesTomados

sumarTodosLosViajes :: [Viaje] -> Int
sumarTodosLosViajes = sum.map costo 

-- alternativaSumarTodosLosViajes :: [Viaje] -> Int
-- alternativaSumarTodosLosViajes (x:xs) = costo.x + sumarTodosLosViajes xs

realizarUnViaje :: [Viaje] -> [Chofer] -> [Chofer]
realizarUnViaje unViaje = efectuarViaje unViaje.choferConMenosViajes.filter (choferPuedeTomarElViaje unViaje)

choferConMenosViajes :: [Chofer] -> Chofer
choferConMenosViajes (primerChofer: segundoChofer: restoChoferes) = choferConMenosViajes ((compararDosChoferes primerChofer segundoChofer):restoChoferes)

compararDosChoferes :: Chofer -> Chofer -> Chofer
compararDosChoferes primerChofer segundoChofer 
		| cantidadViajesRealizados primerChofer > cantidadViajesRealizados segundoChofer = segundoChofer
		| otherwise = primerChofer

cantidadViajesRealizados :: Chofer -> Int
cantidadDeViajesRealizados = length.viajesTomados

efectuarViaje :: Viaje -> Chofer -> Chofer
efectuarViaje unViaje unChofer = unChofer { viajesTomados = (++ unViaje).viajesTomados $ unChofer }

nitoInfy = Chofer { nombre = "Nito Infy", kilometraje = 70000, viajesTomados = viajesInfinitos, condicion = (clienteNLetras 2)

viajesInfinitos = viajeConLucas : viajesInfinitos viajeConLucas
viajeConLucas = [Viaje (11,03,2017) lucas 50]

gongNeg :: (Ord a) => a -> (a->Bool) -> (b->a) -> [b] -> a
gongNeg arg1 arg2 arg3 = max arg1 . head . filter arg2 . map arg3