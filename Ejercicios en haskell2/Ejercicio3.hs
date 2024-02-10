calificaciones :: [Int] -> [String]
calificaciones = map clasificar
  where
    clasificar calificacion
      | calificacion >= 95 = "Excelente"
      | calificacion >= 85 = "Notable"
      | calificacion >= 75 = "Bueno"
      | calificacion >= 70 = "Suficiente"
      | otherwise = "Desempeño insuficiente"

main :: IO ()
main = do
  let notas = [100, 90, 80, 73, 60]
  putStrLn "Calificaciones:"
  mapM_ putStrLn (calificaciones notas)
