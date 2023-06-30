module Parser where

import Evaluator
import Data.List (intercalate)


readFunc :: IO Int
readFunc = do
    putStrLn "Choose an equation to solve:"
    putStrLn "0) y' = y + (1 + x)*y^2"
    putStrLn "1) y'x = y"
    putStrLn "2) y' = 2x"
    func <- fmap (read::String -> F) getLine
    if func <= (length equations)
        then return func
        else do
            putStrLn "No such equation bro.."
            putStrLn "Try again\n"
            readFunc
        
readXRange :: IO (X, X)
readXRange = do
    putStr "Enter the left bound of the differentiation interval: "
    a <- fmap (read :: String -> X) getLine
    putStr "Enter the right bound of the differentiation interval: "
    b <- fmap (read :: String -> X) getLine
    if a <= b
        then return (a, b)
        else do
            putStrLn "It is incorrect interval bro.."
            putStrLn "Try again\n"
            readXRange
    
readY0 :: IO Y
readY0 = do
    putStr "Enter y(x_0) = "
    fmap (read :: String -> Y) getLine

readH :: IO H
readH = do
    putStr "Enter h = "
    fmap (read :: String -> H) getLine

readAcc :: IO Eps
readAcc = do
    putStr "Enter accuracy: "
    fmap (read::String -> Eps) getLine

printRes :: Ress -> IO()
printRes [] = putStrLn "The end:)"
printRes ((points, eps, method) : xs) = do
    putStr   $ "\n################\n\nThe " ++ method ++ " method:\n"    
    putStrLn $ (++) "x: " $ intercalate "\t" $ map (take 6 . show) $ fst $ unzip points
    putStrLn $ (++) "y: " $ intercalate "\t" $ map (take 6 . show) $ snd $ unzip points
    putStrLn $ "Accuracy: " ++ (show eps)
    printRes xs
    
    
    
    
    
    
    
    
    
    
    
    
