module Evaluator
    ( Points, Accuracy, Result, checkInterval, checkMethod, methods,
      funcs, eval, systems, checkIntervals, checkIters, evalSys, list, ResultSys,           dsystems
    ) where

type Func = Double -> Double
type Accuracy = Double
type Points = (Double, Double)
type Result = (Double, Double, Int)
type Method = Int -> Int -> Accuracy -> Points -> Result

type FFunc = Double -> Double -> Double
type ResultSys = ([Double], Int, [Double])

funcs :: [Func]
funcs = [ 
         \x -> x^(3 :: Integer) - x + 4,
         \x -> 1 + x,
         \x -> x^(2 :: Integer) - (exp $ -x)
        ]

dfuncs :: [Func]
dfuncs = [
          \x -> 3*x^(2 :: Integer) - 1,
          \_ -> 1,
          \x -> 2*x + (exp $ -x)
         ]

ddfuncs :: [Func]
ddfuncs = [
           \x -> 6*x, 
           \_ -> 0,
           \x -> 2 - (exp $ -x)
          ]

checkInterval :: Points -> Int -> (String, Bool)
checkInterval p fnum =
    case countRoots f p of
        0 -> ("There are no roots bro..", False)
        1 -> ("", True)
        _ -> ("There are too many roots bro..", False)
    where
        f = funcs !! fnum
        
countRoots :: Func -> Points -> Int
countRoots fun (a,b) = counter (map fun [a, (a + 0.001)..b]) (signum $ fun a) 0 where
    counter :: [Double] -> Double -> Int -> Int
    counter [] _ n = n  
    counter (x:xs) sign n = counter xs (signum x) n + if n_sign == sign then 0 else 1 where
        n_sign = signum x

checkMethod :: Int -> Points -> Int -> Bool
checkMethod fnum interval mnum = case mnum of
    0 -> True
    1 -> checkNewton
    2 -> checkIt
    _ -> False
    where
        df = dfuncs !! fnum
        ddf = ddfuncs !! fnum
        
        (a, b) = interval
        checkNewton = (countRoots df interval == 0) && (countRoots ddf interval == 0) && (signum a /= 0)
    
        lambda = -1 / (maximum $ map (\x -> abs $ df x) [a, (a+0.001)..b])
        
        checkIt = all (< 1) $ map (\x -> abs $ 1 + lambda * (df x)) [a, (a + 0.001)..b]


chords :: Method
chords n fnum eps (a,b) = if abs (f x) <= eps
    then (x, f x, n)
    else chords (n + 1) fnum eps points
    where
        f = funcs !! fnum
            
        x = (a * (f b) - b * (f a)) / ((f b) - (f a))
        points = if (f x) * (f a) < 0 then (a, x) else (x, b)


newton :: Method
newton n fnum eps (a,b) = if (f a) * (ddf a) > 0
    then go n a
    else go n b
    where
        f = funcs !! fnum
        df = dfuncs !! fnum
        ddf = ddfuncs !! fnum

        go :: Int -> Double -> Result
        go i x = if abs (f x) > eps 
            then go (i + 1) $ x  - (f x) / (df x)
            else (x, f x, i)

iters :: Method
iters n fnum eps (a,b) = go n a
    where
        f = funcs !! fnum
        df = dfuncs !! fnum

        lambda = -1 / (maximum $ map (\x -> abs $ df x) [a, (a+0.001)..b])
        
        go i x = if abs (f x) > eps 
            then go (i + 1) $ x + lambda * (f x)
            else (x, f x, i)

methods :: [Method]
methods = [chords, newton, iters]


eval :: Method
eval mnum = (methods !! mnum) 1




systems :: [[FFunc]]
systems = [[
             \x y -> x^(2 :: Integer) + y^(2 :: Integer) - 4,
             \x y -> y - 3*x^(2 :: Integer)
           ],
           [
             \x y -> 0.1*x^(2 :: Integer) + x + 0.2*y^(2 :: Integer) - 0.3,
             \x y -> 0.2*x^(2 :: Integer) + y + 0.1*x*y - 0.7
           ]]

dsystems :: [[(FFunc, FFunc)]]
dsystems = [
            [
             (\x _ -> 2*x, \_ y -> 2*y),
             (\x _ -> -6*x, \_ _ -> 1)
            ],
            [
             (\x _ -> 0.2*x + 1, \_ y -> 0.4*y),
             (\x y -> 0.4*x + 0.1*y, \x _ -> 1 + 0.1*x)
            ]
           ]

checkIntervals :: Points -> Points -> Int -> (String, Bool)
checkIntervals (x1, x2) (y1, y2) snum =
    case 1 {--countRootsSys--} of
        0 -> ("There are no solutions bro..", False)
        1 -> ("", True)
        _ -> ("There are too many solutions bro..", False)
    where
        s = systems !! snum
        f1 = s !! 0
        f2 = s !! 1
        dots = [(x,y) | x <- [x1, (x1 + 0.001)..x2], y <- [y1, (y1 + 0.001)..y2], abs (f1 x y) <= 0.001,  abs (f2 x y) <= 0.001]
        
        countRootsSys = length dots
        
        
checkIters :: Int -> Points -> Points -> Bool
checkIters snum (x1, x2) (y1, y2) = dphi < 1
    where
        ds = dsystems !! snum
        
        dots = [(x,y) | x <- [x1, (x1 + 0.001)..x2], y <- [y1, (y1 + 0.001)..y2]]
        
        [(d1x, d1y), (d2x, d2y)] = ds
        lambda1 = 1 / (maximum $ map (\(x, y) -> abs (d1x x y) + abs (d1y x y)) dots)
        lambda2 = 1 / (maximum $ map (\(x, y) -> abs (d2x x y) + abs (d2y x y)) dots)
        
        dphi = maximum [maximum $ map (\(x, y) -> abs (1 - lambda1 * ((d1x x y) + (d1y x y)))) dots, maximum $ map (\(x, y) -> abs (1 - lambda2 * ((d2x x y) + (d2y x y)))) dots]
        
        
evalSys :: Int -> Int -> Double -> [Double] -> Points -> Points-> ResultSys
evalSys n snum eps (x : y : _) (x1, x2) (y1, y2) = if maximum dev <= eps
    then (new_x, n, dev)
    else evalSys (n + 1) snum eps new_x (x1, x2) (y1, y2)
    where
        dev = zipWith (\a b -> abs $ a - b) [x, y] new_x
        
        ds = dsystems !! snum
        
        dots = [(x,y) | x <- [x1, (x1 + 0.001)..x2], y <- [y1, (y1 + 0.001)..y2]]
        
        [(d1x, d1y), (d2x, d2y)] = ds
        lambda1 = 1 / (maximum $ map (\(x, y) -> abs (d1x x y) + abs (d1y x y)) dots)
        lambda2 = 1 / (maximum $ map (\(x, y) -> abs (d2x x y) + abs (d2y x y)) dots)
        
        
        s = systems !! snum
        [f1, f2] = s
        new_x = zipWith (-) [x, y] [lambda1 * (f1 x y), lambda2 * (f2 x y)]
        
list :: Points -> Points -> Int -> [Points]
list (x1, x2) (y1, y2) snum = filter (\(x,y) -> any (< 0.001) $ map (\f -> abs (f x y)) s) listt
     where
        listt = [(x,y) | x <- [x1, (x1 + 0.001)..x2], y <- [y1, (y1 + 0.001)..y2]]
        s = systems !! snum
        
        
