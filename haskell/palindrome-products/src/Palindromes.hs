module Palindromes (largestPalindrome, smallestPalindrome) where

import Data.List (find, subsequences, (\\))
import Data.Maybe (fromJust)

data IncDec = Inc | Dec

data Length = Short | Long deriving (Show)

type PalindromeIngredients = (Length, Integer)

largestPalindrome, smallestPalindrome ::
  Integer -> Integer -> Maybe (Integer, [(Integer, Integer)])
largestPalindrome = endPalindrome Dec
smallestPalindrome = endPalindrome Inc

endPalindrome ::
  IncDec -> Integer -> Integer -> Maybe (Integer, [(Integer, Integer)])
endPalindrome i l h =
  do xs@((f1,f2):_) <- safeHead . filter nonEmpty . map getFactors $ createPalindromes i l h
     return (f1*f2, xs)
  where getFactors = factorsInRange l h
        nonEmpty = not . null

safeHead :: [a] -> Maybe a
safeHead (x:_) = Just x
safeHead [] = Nothing

createPalindromes :: IncDec -> Integer -> Integer -> [Integer]
createPalindromes i minFactor maxFactor =
  takeWhile inRange . map  makePalindrome $
  iterate succPalindromeIngredients (l, x)
  where minProduct = square minFactor
        maxProduct = square maxFactor
        inRange = case i of Inc -> (<= maxProduct)
                            Dec -> (>= minProduct)
        succPalindromeIngredients = case i of
          Inc -> nextPalindromeIngredients
          Dec -> prevPalindromeIngredients
        start = case i of Inc -> minProduct
                          Dec -> maxProduct
        (l, x) = palindromeIngredients start

makePalindrome :: PalindromeIngredients -> Integer
makePalindrome (l, x) = read . makePalindromeList l $ show x

makePalindromeList :: Length -> [a] -> [a]
makePalindromeList l xs = let xs' = reverse xs
                              xs'' = case l of Short -> drop 1 xs'
                                               Long  -> xs'
                    in xs ++ xs''

nextPalindromeIngredients ::
  PalindromeIngredients -> PalindromeIngredients
nextPalindromeIngredients (l, x) =
  let x' = x+1
      overflow = isPowerOf base x'
      x'' = if overflow
            then case l of
                   Short -> x' `div` base
                   Long  -> x'
        else x'
      l' = if overflow then toggle l else l
  in (l', x'')
  where base = 10

prevPalindromeIngredients ::
  PalindromeIngredients -> PalindromeIngredients
prevPalindromeIngredients (l, x) =
  let x' = x-1
      overflow = isPowerOf base x
      x'' = if overflow
            then case l of
                   Short -> x'
                   Long  -> x * base - 1
        else x'
      l' = if overflow then toggle l else l
  in (l', x'')
  where base = 10

palindromeIngredients :: Integer -> PalindromeIngredients
palindromeIngredients = halfInteger

factorsInRange :: Integer -> Integer -> Integer -> [(Integer, Integer)]
factorsInRange minFactor maxFactor = filter inRange . factorPairs
  where inRange = both (inBounds minFactor maxFactor)

factorPairs :: Integer -> [(Integer, Integer)]
factorPairs x = map (fboth product . splitList factors) . subsequences $ factors
  where splitList l xs = (xs, l \\ xs)
        factors = primeFactors x

inBounds :: Integer -> Integer -> Integer -> Bool
inBounds minInteger maxInteger x = minInteger <= x && x <= maxInteger

halfInteger :: Integer -> (Length, Integer)
halfInteger x = let (l, ds) = half $ show x
                in (l, read ds)

half :: [a] -> (Length, [a])
half xs = let l = length xs
              n = l `div` 2
          in if odd l
             then (Short, take (n+1) xs)
             else (Long, take n     xs)

both :: (a -> Bool) -> (a, a) -> Bool
both f (x, y) = f x && f y

fboth :: (a -> b) -> (a, a) -> (b, b)
fboth f (x, y) = (f x, f y)

isPowerOf :: Integer -> Integer -> Bool
isPowerOf b x = let logx = logBase b' x'
              in x == b ^ (round logx :: Integer)
  where b' = fromIntegral b :: Double
        x' = fromIntegral x :: Double

toggle :: Length -> Length
toggle Short = Long
toggle Long = Short

square :: Num a => a -> a
square x = x * x

primeFactors :: Integer -> [Integer]
primeFactors n = getResult . find done . scanl addFactors (n, []) $ primes
  where done = (1 ==) . fst
        getResult = reverse . snd . fromJust

addFactors :: (Integer, [Integer]) -> Integer -> (Integer, [Integer])
addFactors nfs@(n, fs) p
  | n == 1    = nfs
  | composite = addFactors (q, p:fs) p
  | otherwise = nfs
  where (q, r)    = n `quotRem` p
        composite = r == 0

-- Inspired by haskell.org
primes :: [Integer]
primes = filterPrimes [2..]
  where filterPrimes (p:xs) =
          p : filterPrimes [x | x <- xs, x `mod` p /= 0]
        filterPrimes [] =
          []
