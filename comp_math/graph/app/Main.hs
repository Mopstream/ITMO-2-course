module Main (main) where


import System.IO
import Evaluator
import Parser
import Graphics.Gnuplot.Simple


main :: IO ()
main = do
    hSetBuffering stdin LineBuffering
    hSetBuffering stdout NoBuffering
    task <- readTask
    if task == 0 then do equations
    else do sys
    _ <- getLine
    return ()


equations :: IO ()
equations = do
    fnum <- readFunction
    (interval, mnum) <- get fnum
    eps <- readAccuracy
    ans <- return $ eval mnum fnum eps interval
    printRes ans
    plotFunc [Key Nothing
             ,Title  "My awesome graph on Haskell"
             ,Custom "grid" []
             ] (linearScale 1000 interval) $ funcs !! fnum
             
sys :: IO ()
sys = do
    snum <- readSystem
    (xs, ys) <- readIntervals snum
    x <- readApproximation [xs, ys]
    eps <- readAccuracy
    ans <- return $ evalSys 1 snum eps x xs ys
    printResSys ans
    plotDots [Key Nothing
             ,Title  "My awesome graph on Haskell"
             ,Custom "grid" []
             ,YRange ys
             ,XRange xs
             ] $ list xs ys snum
             
