import Data.Char (toUpper)
import Data.Map.Strict (Map, mapKeys, fromList)

tipoCalificacion :: Int -> String
tipoCalificacion nota
    | nota >= 95 = "Excelente"
    | nota >= 85 = "Notable"
    | nota >= 75 = "Bueno"
    | nota >= 70 = "Suficiente"
    | otherwise = "Desempeño insuficiente"

convertirCalificaciones :: Map String Int -> Map String String
convertirCalificaciones = mapKeys (map toUpper) . fmap tipoCalificacion

main :: IO ()
main = do
    let calificaciones = fromList [("Matematicas", 90), ("Ciencias", 78), ("Historia", 85), ("Fisica", 68)]
    print $ convertirCalificaciones calificaciones