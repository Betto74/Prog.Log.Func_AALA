import qualified Data.Map as Map

applyDiscount :: Double -> Double -> Double
applyDiscount price discount = price - price * discount / 100

applyIVA :: Double -> Double -> Double
applyIVA price percentage = price + price * percentage / 100

priceBasket :: Map.Map Double Double -> (Double -> Double -> Double) -> Double
priceBasket basket function = Map.foldlWithKey' (\acc price discount -> acc + function price discount) 0 basket

main :: IO ()
main = do
  let basket = Map.fromList [(1000, 20), (500, 10), (100, 1)]
  putStrLn $ "El precio de la compra tras aplicar los descuentos es: " ++ show (priceBasket basket applyDiscount)
  putStrLn $ "El precio de la compra tras aplicar el IVA es: " ++ show (priceBasket basket applyIVA)


