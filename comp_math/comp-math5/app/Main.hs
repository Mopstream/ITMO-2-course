module Main (main) where

import System.IO
import Evaluator
import Parser
import Graphics.Gnuplot.Simple


main :: IO ()
main = do
    hSetBuffering stdin LineBuffering
    hSetBuffering stdout NoBuffering
    points <- readPoints
    x <- readX
    (funcs, delts, x) <- return $ eval points x
    
    printRes (funcs, delts, x)
    
    plotFuncs [Key Nothing
             ,Title  "The best interpolation ever"
             ,Custom "grid" []
             ] (linearScale 1000 ((fst $ minimum points) - 0.01, (fst $ maximum points) + 0.01)) funcs
    _ <- getLine
    return ()
    
