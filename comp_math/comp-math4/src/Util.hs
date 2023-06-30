module Util where

det3 :: (Double, Double, Double, 
         Double, Double, Double,
         Double, Double, Double) -> Double 
det3 (a11, a12, a13,
      a21, a22, a23,
      a31, a32, a33) = a11*a22*a33 + a12*a23*a31 + a13*a32*a21
                     - a11*a23*a32 - a22*a13*a31 - a33*a12*a21
                     
                     
det4 :: (Double, Double, Double, Double,
         Double, Double, Double, Double,
         Double, Double, Double, Double,
         Double, Double, Double, Double) -> Double
det4 (a11, a12, a13, a14,
      a21, a22, a23, a24,
      a31, a32, a33, a34,
      a41, a42, a43, a44)
    
    = a11 * det3 (a22, a23, a24,
                  a32, a33, a34,
                  a42, a43, a44)
                         
    - a21 * det3 (a12, a13, a14,
                  a32, a33, a34,
                  a42, a43, a44)
                         
    + a31 * det3 (a12, a13, a14,
                  a22, a23, a24,
                  a42, a43, a44)
                          
    - a41 * det3 (a12, a13, a14,
                  a22, a23, a24,
                  a32, a33, a34)
