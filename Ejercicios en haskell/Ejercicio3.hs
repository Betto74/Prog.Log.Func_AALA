import Data.Map.Strict (Map, fromListWith)

longitudesPalabras :: String -> Map String Int
longitudesPalabras = fromListWith max . map (\x -> (x, length x)) . words

main :: IO ()
main = do
    let frase = "Esta es una frase de ejemplo para probar la función."
    print $ longitudesPalabras frase
