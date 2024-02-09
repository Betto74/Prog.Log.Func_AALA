aplicaFuncionLista :: (a -> b) -> [a] -> [b]
aplicaFuncionLista _ [] = []
aplicaFuncionLista f (x:xs) = f x : aplicaFuncionLista f xs

cuadrado :: Num a => a -> a
cuadrado n = n * n

main :: IO ()
main = print $ aplicaFuncionLista cuadrado [2, 4, 5, 9]
