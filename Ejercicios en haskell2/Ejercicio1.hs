import Text.Printf

sine :: Double -> Double
sine x = sin x

cosine :: Double -> Double
cosine x = cos x

tangent :: Double -> Double
tangent x = tan x

exponential :: Double -> Double
exponential x = exp x

naturalLogarithm :: Double -> Double
naturalLogarithm x = log x

printTable :: (Double -> Double) -> Double -> IO ()
printTable func value = do
  putStrLn "Valor | Resultado"
  putStrLn "-----------------"
  mapM_ (printRow func) [1..value]

printRow :: (Double -> Double) -> Double -> IO ()
printRow func x = printf "%5.2f | %10.5f\n" x (func x)

main :: IO ()
main = do
  putStrLn "Elija la función a calcular:"
  putStrLn "1. Seno"
  putStrLn "2. Coseno"
  putStrLn "3. Tangente"
  putStrLn "4. Exponencial"
  putStrLn "5. Logaritmo neperiano"
  choice <- getLine
  putStrLn "Ingrese el valor:"
  value <- readLn :: IO Double
  case choice of
    "1" -> printTable sine value
    "2" -> printTable cosine value
    "3" -> printTable tangent value
    "4" -> printTable exponential value
    "5" -> printTable naturalLogarithm value
    _   -> putStrLn "Opción no válida"
