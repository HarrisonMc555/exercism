module ComplexNumbers
(Complex,
 conjugate,
 abs,
 real,
 imaginary,
 mul,
 add,
 sub,
 div,
 complex) where

import Prelude hiding (div, abs)

-- Data definition -------------------------------------------------------------
data Complex a = Complex { realPart :: a
                         , imagPart :: a
                         } deriving (Eq, Show)

complex :: (a, a) -> Complex a
complex (r, i) = Complex r i

cartesian :: Complex a -> (a, a)
cartesian c = (realPart c, imagPart c)

-- unary operators -------------------------------------------------------------
conjugate :: Num a => Complex a -> Complex a
conjugate c = let r = realPart c
                  i = negate $ imagPart c
              in complex (r, i)

abs :: Floating a => Complex a -> a
abs c = let (r, i) = cartesian c
        in sqrt $ square r + square i

real :: Num a => Complex a -> a
real = realPart

imaginary :: Num a => Complex a -> a
imaginary = imagPart

negate' :: Num a => Complex a -> Complex a
negate' c = let (r, i) = cartesian c
            in complex (negate r, negate i)

reciprocal :: Fractional a => Complex a -> Complex a
reciprocal c = let (r, i) = cartesian c
                   numerator = complex (r, negate i)
                   denominator = square r + square i
               in scale (1 / denominator) numerator

-- binary operators ------------------------------------------------------------
mul :: Num a => Complex a -> Complex a -> Complex a
mul c1 c2 = let (r1, i1) = cartesian c1
                (r2, i2) = cartesian c2
                r = (r1 * r2) - (i1 * i2)
                i = (r1 * i2) + (r2 * i1)
            in complex (r, i)

add :: Num a => Complex a -> Complex a -> Complex a
add c1 c2 = let (r1, i1) = cartesian c1
                (r2, i2) = cartesian c2
                r = r1 + r2
                i = i1 + i2
            in complex (r, i)

sub :: Num a => Complex a -> Complex a -> Complex a
sub c1 c2 = add c1 (negate' c2)

div :: Fractional a => Complex a -> Complex a -> Complex a
div c1 c2 = c1 `mul` reciprocal c2

scale :: Num a => a -> Complex a -> Complex a
scale k c = let (r, i) = cartesian c
                in complex (r*k, i*k)

-- helper ----------------------------------------------------------------------
square :: Num a => a -> a
square x = x * x
