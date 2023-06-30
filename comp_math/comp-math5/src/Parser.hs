module Parser where
    
import Evaluator
import Data.List.Split (splitOn)
import Data.List (intercalate)


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
    
readX :: IO X
readX = do
    putStr "Enter x: "
    x <- fmap (read :: String -> Double) getLine
    return x

printRes :: Res -> IO()
printRes (funcs, delts, x) = do
    putStr "\n################\n\nDelts:\n"
    putStrLn $ intercalate "\n" $ map ((intercalate "\t") . map (take 6   . show)) delts
    putStr "\nLagrange(x) = "
    putStrLn $ show $ (funcs !! 0) x
    putStr "Gauss(x) = "
    putStrLn $ show $ (funcs !! 1) x
