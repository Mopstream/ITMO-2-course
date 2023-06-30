module Main (main) where

import System.IO (stdin, stdout, hSetBuffering, BufferMode( NoBuffering ))
import Lib


readLimits :: IO (Double, Double)
readLimits = do
    putStr "Enter left integration limit: "
    a_s <- getLine
    a <- return $ read $ a_s
    putStr "Enter right integration limit: "
    b_s <- getLine
    b <- return $ read $ b_s
    return (a,b)

readMethod :: IO (Method, Integer)
readMethod = do
    putStrLn "Choose integration method:"
    putStrLn "0) - left-rectangles"
    putStrLn "1) - middle-rectangles"
    putStrLn "2) - right-rectangles"
    putStrLn "3) - trapeze"
    putStrLn "4) - Simpson"
    m_s <- getLine
    m   <- return $ read m_s
    return $ parseMethod m

readFunction :: IO Func
readFunction = do
    putStrLn "Choose function to integrate:"
    putStrLn "0) -3*x^3 - 5*x^2 + 4*x - 2"
    putStrLn "1) x + 1"
    putStrLn "2) sin(x) + cos(x)"
    putStrLn "3) cos(x) - 12x^3"
    f_s <- getLine
    f   <- return $ read f_s
    return $ parseFunction f
    
readAccuracy :: IO Double
readAccuracy = do
    putStr "Enter accuracy: "
    ac_s <- getLine
    return $ read ac_s

printRes :: (Double, Integer) -> IO()
printRes (res, n) = do
    putStr "Integral = "
    print res
    putStr "\nNeeded splits = "
    print n

main :: IO ()
main = do
    hSetBuffering stdin NoBuffering
    hSetBuffering stdout NoBuffering
    f           <- readFunction
    points      <- readLimits
    (method, k) <- readMethod
    accuracy    <- readAccuracy
    results     <- return $ eval f method k points accuracy
    printRes results
    
