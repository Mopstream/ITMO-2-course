module Evaluator where

import Data.List (zipWith4)

type X       = Double
type Y       = Double
type Func    = X -> Y -> Double
type H       = Double
type Eps     = Double
type Order   = Integer
type F       = Int
type Point   = (X, Y)    
type Points  = [Point]
type Res     = (Points, Eps, String)
type Ress    = [Res]
type Method  = F -> H -> (X, X) -> Y -> Eps -> Res
type Methods = [Method]
type Form    = Func -> H -> Point -> Point

equations :: [Func]
equations = [
                \x y -> 1 - 3*x + 0.5*y/(x+2)
               ,\x y -> y / x
               ,\x y -> 2*x
            ]

runge :: Points -> Points -> Order -> Eps
runge p1 p2 p = ((snd $ p1 !! 1) - (snd $ p2 !! 2)) / (2^p - 1)

template :: Form -> Order -> String -> Method
template form ord s fnum h (x0, xn) y0 acc = template' h where
    f = equations !! fnum
    template' d = if eps < acc then (p1, eps, s) else template' (d/2) where
        eps = runge p1 p2 ord        
        
        p1 = take n1 $ iterate (form f h) (x0, y0)
        n1 = (+) 1 $ floor $ (xn - x0) / d
    
        p2 = take n2 $ iterate (form f h) (x0, y0)
        n2 = (+) 1 $ floor $ (xn - x0) / (d / 2)

eiler :: Method
eiler = template eiler_form 1 "eiler"
    
eiler_form :: Form
eiler_form f h (x, y) = (next_x, next_y) where
    next_x = x + h
    next_y = y + h * (f x y)

mod_eiler :: Method
mod_eiler = template mod_eiler_form 2 "modified eiler"
    
mod_eiler_form :: Form
mod_eiler_form f h (x, y) = (next_x, next_y) where
    next_x = x + h
    next_y = y + h / 2 * (f x y + f next_x e_y)
    e_y = y + h * (f x y)
    
    
runge_kutt :: Method
runge_kutt = template runge_kutt_form 4 "runge-kutt"
    
runge_kutt_form :: Form
runge_kutt_form f h (x, y) = (next_x, next_y) where
    next_x = x + h
    next_y = y + (k1 + 2 * k2 + 2 * k3 + k4) / 6
    k1 = h * (f x y)
    k2 = h * (f (x + h / 2) (y + k1 / 2))
    k3 = h * (f (x + h / 2) (y + k2 / 2))
    k4 = h * (f (x + h) (y + k3))


miln :: Method
miln fnum h (x0, xn) y0 acc = (take n seq, acc, "miln") where
    n = (+) 1 $ floor $ (xn - x0) / h
    f = equations !! fnum
    beg = take 4 $ iterate (runge_kutt_form f h) (x0, y0)

    seq = beg ++ (zipWith4 form seq (tail seq) (drop 2 seq) (drop 3 seq))
    form p4 p3 p2 p1 = form' pred where
        y4 = snd p4
        y2 = snd p2
        x0 = h + (fst p1)
        fp = uncurry f
        pred = y4 + 4*h/3 *(2*(fp p3) - (fp p2) + 2 * (fp p1))
        
        form' pred = if abs (pred - new) < acc then (x0, new) else form' new where
            new = y2 + h/3*((fp p2) + 4*(fp p1) + (f x0 pred))
        
    

methods :: Methods
methods = [eiler, mod_eiler, runge_kutt, miln]
  
eval :: F -> H -> (X, X) -> Y -> Eps -> Ress
eval f h xs y acc = map (\m -> m f h xs y acc) methods

















