module Parser where
    
import Evaluator

readFunction :: IO Int
readFunction = do
    putStrLn "Choose an equation to solve:"
    putStrLn "0) x^3 - x + 4 = 0"
    putStrLn "1) x + 1 = 0"
    putStrLn "2) x^2 - e^(-x) = 0"
    func <- fmap (read::String -> Int) getLine
    if func <= (length funcs)
        then return func
        else do
            putStrLn "No such function bro.."
            putStrLn "Try again\n"
            readFunction
            
readInterval :: Int -> IO Points
readInterval f = do
    putStr "Enter the left bound of the root isolation interval: "
    a <- fmap (read :: String -> Double) getLine
    putStr "Enter the right bound of the root isolation interval: "
    b <- fmap (read :: String -> Double) getLine
    case checkInterval (a,b) f of
        (_, True)  -> return (a,b)
        (s, False) -> do
            putStrLn s
            putStrLn "Try again\n"
            readInterval f
         
            
readAccuracy :: IO Accuracy
readAccuracy = do
    putStr "Enter accuracy: "
    fmap (read::String -> Accuracy) getLine

    
readMethod :: IO Int
readMethod = do
    putStrLn "Choose evaluations method:"
    putStrLn "0) - chords"
    putStrLn "1) - Newton"
    putStrLn "2) - simple-iters"
    method <- fmap (read::String -> Int) getLine
    if method <= (length methods)
        then return method 
        else do
            putStrLn "No such method bro.."
            putStrLn "Try again\n"
            readMethod


get :: Int -> IO (Points, Int)
get fnum = do
    interval <- readInterval fnum
    mnum     <- readMethod
    if checkMethod fnum interval mnum 
    then return (interval, mnum)
    else do
        putStrLn "You shouldn't use this method on this interval."
        putStrLn "It does not converge bro.."
        putStrLn "Try again\n"
        get fnum

printRes :: Result -> IO()
printRes (x, res, n) = do
    putStr "\n################\n\nx = "
    print x
    putStr "f(x) = "
    print $ res
    putStr "Needed iters = "
    print n
    
    
readTask :: IO Int
readTask = do 
    putStrLn "Choose task:"
    putStrLn "0) - equations"
    putStrLn "1) - systems of equations"
    task <- fmap (read::String -> Int) getLine
    if task <= 1 
    then return task
    else do
        putStrLn "No such task bro.."
        putStrLn "Try again\n"
        readTask
    
readSystem :: IO Int
readSystem = do
    putStrLn "Choose an system to solve:"
    putStrLn "0) / x^2 + y^2 = 4"
    putStrLn "  <"
    putStrLn "   \\ y = 0.5x^2\n"
    putStrLn "1) / 0.1x^2 + x + 0.2y^2 - 0.3 = 0"
    putStrLn "  <"
    putStrLn "   \\ 0.2x^2 + y + 0.1xy - 0.7 = 0"
    system <- fmap (read::String -> Int) getLine
    if system <= (length systems)
        then return system
        else do
            putStrLn "No such system bro.."
            putStrLn "Try again\n"
            readSystem
            
            
readIntervals :: Int -> IO (Points, Points)
readIntervals snum = do
    putStr "Enter the left Xbound of the root isolation interval: "
    x1 <- fmap (read :: String -> Double) getLine
    putStr "Enter the right Xbound of the root isolation interval: "
    x2 <- fmap (read :: String -> Double) getLine
    putStr "Enter the left Ybound of the root isolation interval: "
    y1 <- fmap (read :: String -> Double) getLine
    putStr "Enter the right Ybound of the root isolation interval: "
    y2 <- fmap (read :: String -> Double) getLine
    case checkIntervals (x1, x2) (y1, y2) snum of
        (_, True)  -> if checkIters snum (x1, x2) (y1, y2) 
                      then return ((x1, x2), (y1, y2))
                      else do
                        putStrLn "Simple iterations method does not converge on this interval."
                        putStrLn "Try again\n"
                        readIntervals snum
        (s, False) -> do
            putStrLn s
            putStrLn "Try again\n"
            readIntervals snum
       
readApproximation :: [Points] -> IO [Double]
readApproximation lims = do
    putStr "Enter coords of initial approximation separated by a space: "
    app <- fmap (map (read :: String -> Double) . words) getLine
    if all (\(x, (x1, x2)) -> x1 <= x && x <= x2) $ zip app lims
    then return app
    else do
        putStrLn "Your vector is out of scope bro.."
        putStrLn "Try again\n"
        readApproximation lims 
        
printResSys :: ResultSys -> IO ()
printResSys (x, n, dev) = do
    putStr "\n################\n\nx = "
    print x
    putStr "Needed iters = "
    print n
    putStr "Deviations vector: "
    print dev
    
