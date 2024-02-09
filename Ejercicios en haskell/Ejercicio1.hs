aplicarDescuento :: Float -> Float -> Float
aplicarDescuento precio descuento = precio - (precio * descuento / 100)

aplicarIVA :: Float -> Float -> Float
aplicarIVA precio iva = precio + (precio * iva / 100)


precioFinalCesta :: [(Float, Float)] -> (Float -> Float -> Float) -> Float
precioFinalCesta [] _ = 0
precioFinalCesta ((precio, porcentaje):resto) funcion =
    funcion precio porcentaje + precioFinalCesta resto funcion

main :: IO ()
main = do
    let cesta = [(100, 10), (50, 5), (30, 0)]
    let precioFinalConDescuento = precioFinalCesta cesta aplicarDescuento
    let precioFinalConIVA = precioFinalCesta cesta aplicarIVA
    putStrLn $ "Precio final con descuento: " ++ show precioFinalConDescuento
    putStrLn $ "Precio final con IVA: " ++ show precioFinalConIVA
