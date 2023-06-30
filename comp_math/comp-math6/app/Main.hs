module Main (main) where

import System.IO
import Evaluator
import Parser
import Graphics.Gnuplot.Simple


main :: IO ()
main = do
    hSetBuffering stdin LineBuffering
    hSetBuffering stdout NoBuffering
    f   <- readFunc
    xs  <- readXRange
    y0  <- readY0
    h   <- readH
    acc <- readAcc
    
    ress <- return $ eval f h xs y0 acc
    printRes ress
    
    plotLists [Key Nothing
             ,Title  "Last lab YEAHHHHH!!!!"
             ,Custom "grid" []
             ] $ map (\(x, _, _) -> x) ress
    _ <- getLine
    return ()
    
