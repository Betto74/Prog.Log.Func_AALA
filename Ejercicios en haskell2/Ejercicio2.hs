import Data.List (filter)

filtraLista :: (a -> Bool) -> [a] -> [a]
filtraLista funcion lista = filter funcion lista

mayorQueDiez :: Int -> Bool
mayorQueDiez n = n > 10

main :: IO ()
main = print (filtraLista mayorQueDiez [1, 2, 3, 4, 5, 6, 11, 12, 13])
