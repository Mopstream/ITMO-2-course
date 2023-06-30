module Lib
    ( eval, parseFunction, parseMethod, Func, Method
    ) where

type Func = Double -> Double
type Accuracy = Double
type Points = (Double, Double)
type Method = Double -> Func -> Points -> Double

func0 :: Func
func0 x = -3*x^(3 :: Integer) -5*x^(2 :: Integer) + 4*x - 2

func1 :: Func
func1 x = 1 + x

func2 :: Func
func2 x = (sin x) + (cos x)

func3 :: Func
func3 x = (cos x) - 12*x^(3 :: Integer)


rect :: Double -> Double -> Method
rect l r n f (a, b) = diff * (sum $ map ((\x -> f (a + x*diff))) [l..r]) where
    diff = (b - a) / n
    
left_rect :: Method
left_rect n = rect 0.0 (n - 1) n 

right_rect :: Method
right_rect n = rect 1.0 n n

middle_rect :: Method
middle_rect n = rect 0.5 (n - 0.5) n

trapeze :: Method
trapeze n f (a, b) = (left_rect n f (a, b)) + ((f b) - (f a))/ 2 * diff where
    diff = (b - a)/n

simpson :: Method
simpson n f (a, b) = diff/3 * (f a + f b + 2 * (sum $ map (\x -> f (a + x*diff)) [2,4..n-2]) + 4 * (sum $ map (\x -> f (a + x*diff)) [1,3..n-1])) where
    diff = (b - a)/n


runge :: (Double, Double) -> Integer -> Accuracy -> Bool
runge (i1, i2) k eps = ((i1 - i2) / (2^k - 1)) < eps


parseFunction :: Int -> Func
parseFunction func = case func of
    0 -> func0
    1 -> func1
    2 -> func2
    3 -> func3
    _ -> error "no such function bro"

parseMethod ::  Int -> (Method, Integer)
parseMethod method = case method of
    0 -> (left_rect, 2)
    1 -> (middle_rect, 2)
    2 -> (right_rect, 2)
    3 -> (trapeze, 2)
    4 -> (simpson, 4)
    _ -> error "no such method bro"
    
eval :: Func -> Method -> Integer -> Points -> Accuracy -> (Double, Integer)
eval f method k points = eval' 4 (method 4 f points) f method k points

eval' :: Double -> Double -> Func -> Method -> Integer -> Points -> Accuracy -> (Double, Integer)
eval' n i1 f method k points eps = if (runge (i1, i2) k eps) then (i2, (round (n*2))) else (eval' (n*2) i2 f method k points eps) where
    i2 = method (n*2) f points

