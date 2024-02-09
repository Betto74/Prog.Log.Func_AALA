-- Función para calcular la media de una lista de números
media :: (Fractional a) => [a] -> a
media xs = sum xs / fromIntegral (length xs)

-- Función para calcular la desviación estándar de una lista de números
desviacionEstandar :: (Floating a) => [a] -> a
desviacionEstandar xs =
  let m = media xs
      n = fromIntegral $ length xs
      squaredDiffs = map (\x -> (x - m) ^ 2) xs
  in sqrt (sum squaredDiffs / n)

-- Función para determinar si un valor es atípico en función de la media y la desviación estándar
atipico :: (Floating a, Ord a) => [a] -> a -> Bool
atipico muestra n =
  let mediaMuestra = media muestra
      desviacion = desviacionEstandar muestra
      puntuacion = (n - mediaMuestra) / desviacion
  in puntuacion < -3 || puntuacion > 3

-- Función para filtrar los datos atípicos de una muestra
datosAtipicos :: (Floating a, Ord a) => [a] -> [a]
datosAtipicos muestra = filter (atipico muestra) muestra

main :: IO ()
main = print $ datosAtipicos [10.0, 15.0, 20.0, 25.0, 30.0, 35.0, 40.0, 100.0, 105.0, 110.0, 115.0, 500.0]
