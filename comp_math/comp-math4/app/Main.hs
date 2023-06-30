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
    (func, s, epsilon, pir) <- return $ chooseFunc points
    printRes (func, s, epsilon, pir)
    (dots, xs, ys) <- return $ listDots points func
    plotFunc [Key Nothing
             ,Title  "The best approximation ever"
             ,Custom "grid" []
             {--,XRange xs
             ,YRange ys--}
             ] (linearScale 1000 xs) func
    plotDots [Key Nothing
             ,Title  "The best approximation ever"
             ,Custom "grid" []
             {--,XRange xs
             ,YRange ys--}
             ] $ dots
    _ <- getLine
    return ()
    
