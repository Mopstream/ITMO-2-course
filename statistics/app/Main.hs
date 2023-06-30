module Main (main) where


import Data.List (sort, group, intercalate)
import System.IO
import Graphics.Gnuplot.Simple

expected_value :: [Double] -> Double
expected_value l = (sum l) / n

list :: [Double]
list = [0.83, 0.73, -0.48, 0.00, -1.35, 1.59, 0.31, 0.17, 0.59, -0.45, 1.35, 1.60, -0.30, -0.18, -0.24, -1.73, 0.51, 0.03, 0.26, 1.70]

n = realToFrac $ length list

variation_series :: [Double]
variation_series = sort list

maxim = last variation_series
minim = head variation_series
amplitude = maxim - minim

mean = expected_value list

standart_deviation = (expected_value $ map (^2) list) - mean^2
fixed_sko = (standart_deviation) * (sqrt $ n/(n-1))

f_n :: Double -> Double
f_n x = n_x / n where
    n_x = realToFrac $ length $ filter (< x) list

print_F_n :: IO()
print_F_n = do
    putStrLn "F_n(x):"
    help (group variation_series) Nothing
    where
        help (x : xs) Nothing = do
            putStrLn $ (show $ f_n $ head x) ++ ",\tx <= " ++ (show $ head x)
            help xs (Just $ show $ head x)
        help (x:xs) (Just s) = do
            putStrLn $ (show $ f_n $ head x) ++ ",\t" ++ s ++ " < x <= " ++ (show $ head x)
            help xs (Just $ show $ head x)
            
        help [] (Just s) = putStrLn $ "1, x >\t" ++ s
        help _ _ = putStrLn ""

splitted = zip tmp (tail tmp) where
    m = realToFrac $ floor $ 1.5 + (1 + logBase 2 n)
    first = minim - h/2
    tmp = [first, first+h..(first + m*h)]

polygonList = map (\(x,y) -> length $ filter (\t -> x <= t && t < y) list) splitted

h = amplitude / (1 + logBase 2 n)

hystogramm :: Double -> Double
hystogramm x = case ((x - near) < (h - 0.005)) && (near /= maxim) && (near >= minim - h/2) of
    True -> count
    False -> 0
    where
    i = realToFrac $ floor $ (x - minim) / h + 0.5
    near = minim - h/2 + i*h
    next = near + h
    
    count = (realToFrac $ length $ filter (\x -> near <= x && x < next) list) / h

main :: IO ()
main = do
    putStrLn "Вариационный ряд:"
    printList "\t" variation_series
    putStr "\nx_min, x_max: "
    printList ", " [minim, maxim]
    putStrLn $ "Размах: " ++ (show amplitude)
    putStrLn $ "Математическое ожидание M(x) = " ++ (show mean)
    putStrLn $ "Среднеквадратическое отклонение δ = " ++ (show standart_deviation)
    putStrLn $ "Исправленное ско = " ++ (show fixed_sko)
    print_F_n
    plotFunc [Key Nothing
             ,Title  "F*_n(x)"
             ,Custom "grid" []    
             ,XLabel "x"
             ,YLabel "p*(X < x)" 
             ] (linearScale 1000 (minim, maxim + 0.01)) f_n
    
    plotList [Key Nothing
             ,Title  "n(x)"
             ,Custom "grid" []
             ,XLabel "x"
             ,YLabel "n"
             ] polygonList
    plotFunc [Key Nothing
             ,Title  "Awesome hystogramm"
             ,Custom "grid" []    
             ,XLabel "x"
             ,YLabel "n_i / h" 
             ] (linearScale 1000 (minim - h/2-0.005, maxim+0.11)) hystogramm
    _ <- getLine
    return ()
    
printList :: String -> [Double] -> IO ()
printList sep list = putStrLn $ intercalate sep $ map show list
