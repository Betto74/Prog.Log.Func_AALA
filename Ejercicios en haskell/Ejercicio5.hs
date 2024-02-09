moduloVector :: Floating a => [a] -> a
moduloVector = sqrt . sum . map (^2)

main :: IO ()
main = do
    let vector = [3.0, 4.0]
    putStrLn $ "El módulo del vector " ++ show vector ++ " es " ++ show (moduloVector vector)
