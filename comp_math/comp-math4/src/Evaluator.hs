module Evaluator where

import Util
import Data.List (intercalate, minimumBy)

type Func   = Double -> Double
type X      = Double
type Y      = Double
type Point  = (X, Y)
type Points = [Point]
type Params = [Double]
type Method = Int
type Res    = (Func, String, Double, Double)

evalS :: Points -> Func -> Double
evalS points phi = sum $ map (\(x, y) -> (phi x - y) ^ 2) points

eps :: Points -> Func -> Double
eps points phi = (\x -> sqrt (x / n)) $ evalS points phi where
    n = realToFrac $ length points

getLinParams :: Points -> Params
getLinParams points = [d1/d, d2/d] where
    d  = xx * n - x ^ 2
    d1 = xx * y - x * xy
    d2 = xy * n - x * y
    
    n  = realToFrac $ length points
    x  = sum $ map (\(x, _) -> x)     points
    xx = sum $ map (\(x, _) -> x^2)   points
    y  = sum $ map (\(_, y) -> y)     points
    xy = sum $ map (\(x, y) -> x * y) points
    
getLinFunc :: Points -> (Func, String)
getLinFunc points = ((\x -> a + b*x), intercalate " " [show a, "+", show b, "* x"]) where
    (a : b : _) = getLinParams points
    
getPol2Params :: Points -> Params
getPol2Params points = [d1/d, d2/d, d3/d] where
    d  = det3 (n, x1, x2, 
              x1, x2, x3, 
              x2, x3, x4)
              
    d1 = det3 (x0y, x1, x2, 
               x1y, x2, x3, 
               x2y, x3, x4)
             
    d2 = det3 (n, x0y, x2,
              x1, x1y, x3, 
              x2, x2y, x4)
             
    d3 = det3 (n, x1, x0y, 
              x1, x2, x1y,
              x2, x3, x2y)
    
    n   = realToFrac $ length points
    x1  = sum $ map (\(x, _) -> x)         points
    x2  = sum $ map (\(x, _) -> x^2)       points
    x3  = sum $ map (\(x, _) -> x^3)       points
    x4  = sum $ map (\(x, _) -> x^4)       points
    x0y = sum $ map (\(_, y) -> y)         points
    x1y = sum $ map (\(x, y) -> x * y)     points
    x2y = sum $ map (\(x, y) -> x * x * y) points
    
getPol2Func :: Points -> (Func, String)
getPol2Func points = ((\x -> a + b*x + c*x^2), intercalate " " [show a, "+", show b, "* x +", show c, "* x^2"]) where
    (a : b : c : _) = getPol2Params points
    
getPol3Params :: Points -> Params
getPol3Params points = [d1/d, d2/d, d3/d, d4/d] where
    d  = det4 (n, x1, x2, x3,
              x1, x2, x3, x4,
              x2, x3, x4, x5,
              x3, x4, x5, x6)
              
    d1 = det4 (x0y, x1, x2, x3,
               x1y, x2, x3, x4,
               x2y, x3, x4, x5,
               x3y, x4, x5, x6)
             
    d2 = det4 (n, x0y, x2, x3,
              x1, x1y, x3, x4,
              x2, x2y, x4, x5,
              x3, x3y, x5, x6)
             
    d3 = det4 (n, x1, x0y, x3,
              x1, x2, x1y, x4,
              x2, x3, x2y, x5,
              x3, x4, x3y, x6)
    
    d4 = det4 (n, x1, x2, x0y,
              x1, x2, x3, x1y,
              x2, x3, x4, x2y,
              x3, x4, x5, x3y)         
    
    n   = realToFrac $ length points
    x1  = sum $ map (\(x, _) -> x)             points
    x2  = sum $ map (\(x, _) -> x^2)           points
    x3  = sum $ map (\(x, _) -> x^3)           points
    x4  = sum $ map (\(x, _) -> x^4)           points
    x5  = sum $ map (\(x, _) -> x^5)           points
    x6  = sum $ map (\(x, _) -> x^6)           points    
    x0y = sum $ map (\(_, y) -> y)             points
    x1y = sum $ map (\(x, y) -> x * y)         points
    x2y = sum $ map (\(x, y) -> x * x * y)     points
    x3y = sum $ map (\(x, y) -> x * x * x * y) points
    
getPol3Func :: Points -> (Func, String)
getPol3Func points = ((\x -> a + b*x + c*x^2 + d*x^3), intercalate " " [show a, "+", show b, "* x +", show c, "* x^2 +", show d, "* x^3"]) where
    (a : b : c : d : _) = getPol3Params points


getExpParams :: Points -> Params
getExpParams points = [exp a, b] where
    (a:b:_) = getLinParams $ map (\(x,y) -> (x, log y)) points

getExpFunc :: Points -> (Func, String)
getExpFunc points = ((\x -> a * (exp $ b*x)), intercalate " " [show a, "* e^(", show b,"* x)"]) where
    (a : b : _) = getExpParams points

getLnParams :: Points -> Params
getLnParams points = getLinParams $ map (\(x,y) -> (log x, y)) points
    
getLnFunc :: Points -> (Func, String)
getLnFunc points = ((\x -> a + b * (log x)), intercalate " " [show a, "+", show b,"* ln x"]) where
    (a : b : _) = getLnParams points
    
getPowParams :: Points -> Params
getPowParams points = [exp a, b] where
    (a:b:_) = getLinParams $ map (\(x,y) -> (log x, log y)) points
    
getPowFunc :: Points -> (Func, String)
getPowFunc points = ((\x -> a * (b ** x)), intercalate " " [show a, "*", show b,"^ x"]) where
    (a : b : _) = getPowParams points

pirson :: Points -> Double
pirson points = xy / (sqrt (xx * yy)) where
    n   = realToFrac $ length points
    xs = (sum $ map (\(x, _) -> x) points) / n
    ys = (sum $ map (\(_, y) -> y) points) / n
    
    xy = sum $ map (\(x, y) -> (x - xs) * (y - ys)) points
    xx = sum $ map (\(x, _) -> (x - xs) ^ 2)        points
    yy = sum $ map (\(_, y) -> (y - ys) ^ 2)        points


chooseFunc :: Points -> Res
chooseFunc points = minimumBy comp ress where
    ress = map getRes [getLinFunc, getPol2Func, getPol3Func, getExpFunc, getLnFunc, getPowFunc]
    
    getRes :: (Points -> (Func, String)) -> Res
    getRes f = (func, str, eps points func, pirson points) where
        (func, str)  = f points
    
    comp = \(_,_,a,_) (_,_,b,_) -> case (isNaN a, isNaN b) of
        (True, True) -> EQ
        (True, _)    -> GT
        (_, True)    -> LT
        (_,_)        -> compare a b    
        
listDots :: Points -> Func -> (Points, Point, Point)
listDots points func = (filter filterFunc raw_list, (x_min, x_max), (y_min, y_max)) where

    raw_list = [(x,y) | x <- [x_min, x_min+0.001..x_max], y <- [y_min, y_min+0.001..y_max]]
    filterFunc = (\(x,y) -> abs(func x - y) <= 0.01 || any (\(x1, y1) -> sqrt (5 * (x1-x)^2 + (y1 - y)^2) <= 0.1) points)
    
    x_min = (fst $ minimum points) - 1
    x_max = (fst $ maximum points) + 1
        
    y_min = (snd $ minimum points) - 1
    y_max = (snd $ maximum points) + 1
    
