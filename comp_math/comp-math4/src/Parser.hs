module Parser where
    
import Evaluator
import Data.List.Split (splitOn)


readPoints :: IO Points
readPoints = do
    putStrLn "Enter the coordinates of the point separated by a space (like \"1.5 2\")"
    putStrLn "When you get bored, click \"Enter\""
    points <- reader [] 
    if length points == 0
    then do
        putStrLn "Bro, there are no points.."
        readPoints
    else return points where
        
        
        reader points = do
            input <- getLine
            if input == "" 
            then return points
            else do
                (x : y : _) <- return $ map (read :: String -> Double) $ words input
                reader $ (x, y) : points
    

printRes :: Res -> IO()
printRes (_, str, eps, pirs) = do
    putStr "\n################\n\nApproximation function: f(x) = "
    putStrLn str
    putStr "Standard deviation: eps = "
    print $ eps
    putStr "Pearson coefficient: p = "
    print pirs
    
