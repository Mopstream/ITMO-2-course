{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module Evaluator where

import Data.List (intercalate, minimumBy, sort)


type Func   = Double -> Double

type X      = Double
type Y      = Double
type Point  = (X, Y)    
type Points = [Point]
type Res    = ([Func], [[Y]], X)

class Function a where
    (.+.)  :: a -> a -> a
    (.*.)  :: a -> a -> a
    zero   :: a
    one    :: a

    (*.) :: Double -> a -> a
    
    summ :: [a] -> a
    summ  = foldr (.+.) zero

    prodd :: [a] -> a
    prodd = foldr (.*.) one
    
    weighted_summ :: [Double] -> [a] -> a
    weighted_summ ks fs = summ $ zipWith (*.) ks fs

instance Function Func where
    f .+. g = \x -> (f x) + (g x)
    f .*. g = \x -> (f x) * (g x)
    k *. f = \x -> k * (f x)
    zero = \_ -> 0
    one  = \_ -> 1

lacringe :: Points -> Func
lacringe points = weighted_summ ys ls where
    (xs, ys) = unzip points
   
    ls = map terrible_shit xs
    
    terrible_shit x = (1 / denominator) *. numerator where
        numerator   = prodd $ map (\k -> \t -> if k == x then 1 else t - k) xs
        denominator = product $ map (\xj -> if x == xj then 1 else xj - x) xs

gauss :: Points -> Func
gauss points x = (if x > a then firstGauss else secondGauss) spoints x where
    spoints = sort points
    a = fst $ spoints !! ((length spoints - 1) `div` 2)

    
    
firstGauss :: Points -> Func
firstGauss points x =  (y0 .+. summ yi) new_x where
    (xs, ys) = unzip points
    i = ((length xs - 1) `div` 2)
    y0 = \_ -> ys !! i
    x0 = xs !! i
    new_x = (x - x0) / ((xs !! 1) - (xs !! 0))
    
    yi = zipWith transform [0..(i-1)] pols
    
    yy = deltas ys
    
    transform :: Int -> Func -> Func
    transform i0 p = first .+. second where
        i1 = i0 + 1
        i2 = 2*i1 - 1
    
        first :: Func
        first  = ((yy !! i2 !! (i - i0)) / (facts !! i2)) *. p
        second :: Func
        second = ((yy !! (i2 + 1) !! (i - i1)) / (facts !! (i2 + 1))) *. (p .*. (\t -> t - (realToFrac i1)))
    

d :: [Double] -> [Double]
d list = zipWith (-) (tail list) list

deltas :: [Double] -> [[Double]]
deltas list = take (length list) $ iterate d list  
    
facts :: [Double]
facts = scanl (*) 1 [1..]

pols :: [Func]
pols = scanl (\p1 n -> p1 .*. (\t -> t^2 - n^2)) id [1..]


secondGauss :: Points -> Func
secondGauss points x =  (y0 .+. summ yi) new_x where
    (xs, ys) = unzip points
    
    i = ((length xs - 1) `div` 2)
    y0 = \_ -> ys !! i
    x0 = xs !! i
    
    new_x = (x - x0) / ((xs !! 1) - (xs !! 0))
    
    yi = zipWith transform [1..i] pols
    
    transform :: Int -> Func -> Func
    transform i1 p = first .+. second where
        i2 = 2*i1 - 1
    
        first :: Func
        first  = ((yy !! i2 !! (i - i1)) / (facts !! i2)) *. p
        second :: Func
        second = ((yy !! (i2 + 1) !! (i - i1)) / (facts !! (i2 + 1))) *. (p .*. (\t -> t + (realToFrac i1)))
    
    yy = deltas ys

dots :: Points -> Func
dots points x = help points where

    help [] = 0
    help (t : ts) = if abs(x - fst t) < 0.005 then (snd t) else help ts

eval :: Points -> X -> Res
eval points x = ([lacringe points, \x -> gauss points x, dots points], yy, x) where
    yy = deltas $ snd $ unzip $ sort points

