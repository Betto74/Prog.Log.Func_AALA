import Data.List

type Inmueble = (Int, Int, Int, Bool, Char)
type Precio = Float

-- Función para calcular el precio de un inmueble en función de la zona
precioInmueble :: Inmueble -> Precio
precioInmueble (año, metros, habitaciones, garaje, zona)
    | zona == 'A' = (fromIntegral (metros * 1000 + habitaciones * 5000 + if garaje then 15000 else 0)) * (1 - fromIntegral (2024 - año) / 100)
    | zona == 'B' = (fromIntegral (metros * 1000 + habitaciones * 5000 + if garaje then 15000 else 0)) * (1 - fromIntegral (2024 - año) / 100) * 1.5
    | otherwise = 0.0

-- Función para filtrar inmuebles según un presupuesto dado
filtrarPorPresupuesto :: [Inmueble] -> Precio -> [(Inmueble, Precio)]
filtrarPorPresupuesto inmuebles presupuesto = 
    let inmueblesConPrecio = map (\inmueble -> (inmueble, precioInmueble inmueble)) inmuebles
        inmueblesFiltrados = filter (\(_, precio) -> precio <= presupuesto) inmueblesConPrecio
    in inmueblesFiltrados

-- Función para imprimir un inmueble con su precio
mostrarInmueble :: (Inmueble, Precio) -> String
mostrarInmueble ((año, metros, habitaciones, garaje, zona), precio) =
    "Año: " ++ show año ++ ", Metros cuadrados: " ++ show metros ++ ", Habitaciones: " ++ show habitaciones ++ ", Garaje: " ++ show garaje ++ ", Zona: " ++ show zona ++ ", Precio: " ++ show precio

-- Ejemplo de uso:
-- Supongamos que tenemos la lista de inmuebles:
inmuebles :: [Inmueble]
inmuebles = [(2000, 100, 3, True, 'A'),
             (2012, 60, 2, True, 'B'),
             (1980, 120, 4, False, 'A'),
             (2005, 75, 3, True, 'B'),
             (2015, 90, 2, False, 'A')]

-- Y queremos buscar inmuebles con un presupuesto de 100,000
main :: IO ()
main = do
    let presupuesto = 100000
    let inmueblesConPrecio = filtrarPorPresupuesto inmuebles presupuesto
    mapM_ (putStrLn . mostrarInmueble) inmueblesConPrecio
