module Palindromes (largestPalindrome, smallestPalindrome) where

import Data.Numbers.Primes (primeFactors)
import Data.Maybe (fromMaybe)
import Data.List (find,
                  sortBy,
                  groupBy,
                  subsequences,
                  genericLength,
                  (\\))

data IncDec = Inc | Dec

largestPalindrome, smallestPalindrome ::
  Integer -> Integer -> (Integer, [(Integer, Integer)])
largestPalindrome = _endPalindrome Dec
smallestPalindrome = _endPalindrome Inc

endPalindrome
  :: IncDec -> Integer -> Integer -> (Integer, [(Integer, Integer)])
endPalindrome i l h = if l > h then error "maxFactor less than minFactor"
                      else head $ getPalindromes i l h

getPalindromes
  :: IncDec -> Integer -> Integer -> [(Integer, [(Integer, Integer)])]
getPalindromes incDec minFactor maxFactor =
  -- filter (isPalindrome . fst) $ possibleProducts incDec minFactor maxFactor
  possibleProducts incDec minFactor maxFactor

possibleProducts ::
  IncDec -> Integer -> Integer -> [(Integer, [(Integer, Integer)])]
possibleProducts incDec minFactor maxFactor =
  let ps = [(p, (x, y))
           | x <- [minFactor.. maxFactor],
             y <- [x.. maxFactor],
             let p = x*y,
             isPalindrome p]
  in groupByFst $ sortBy sorting ps
  where sorting = fFst compare'
        compare' = case incDec of Inc -> compare
                                  Dec -> flip compare

groupByFst :: Eq a => [(a, b)] -> [(a, [b])]
groupByFst = map extract . groupBy (fFst (==))
  where extract xys@((x,y):_) = (x, map snd xys)

fFst :: (a -> b -> c) -> (a, d) -> (b, e) -> c
fFst f (x, _) (y, _) = f x y

factors :: Integer -> [(Integer, Integer)]
factors p = map makePair $ 1 : primeFactors p ++ [p]
  where makePair x = (x, p `div` x)

factorsInRange :: Integer -> Integer -> Integer -> [(Integer, Integer)]
factorsInRange minFactor maxFactor x = filter inRange $ factors x
  where inRange = both (inBounds minFactor maxFactor)

isPalindrome :: Integer -> Bool
isPalindrome = isPalindromeString . show

isPalindromeString :: String -> Bool
isPalindromeString s = s == reverse s

both :: (a -> Bool) -> (a, a) -> Bool
both f (x, y) = f x && f y

inBounds :: Integer -> Integer -> Integer -> Bool
inBounds minBound maxBound x = minBound <= x && x <= maxBound

--

-- _endPalindrome :: IncDec -> Integer -> Integer -> [(Integer, Integer)]
_endPalindrome ::
  IncDec -> Integer -> Integer -> (Integer, [(Integer, Integer)])
_endPalindrome i l h =
  makePair . head . filter nonEmpty . map getFactors $ _makePalindromes i l h
  where makePair xs@((f1,f2):_) = (f1*f2, xs)
        getFactors = _factorsInRange l h
        nonEmpty = not . null

_makePalindromeDigitsFrom :: IncDec -> Integer -> [Integer]
_makePalindromeDigitsFrom i x = let (ds, l) = half $ show (x*x)
                                    d = read ds
  in map (_makePalindrome l) [d..10*d-1]

_makePalindromes :: IncDec -> Integer -> Integer -> [Integer]
-- _makePalindromes i l h = map (makePalindrome n) xs
--   where xs = case i of Inc -> [l'.. h']
--                        Dec -> [h',h-1.. l']
--         n = genericLength . show $ h'
--         l' = l*l
--         h' = h*h
-- _makePalindromes i l h = map (_makePalindrome Short) xs
--   where xs = [l..h]
_makePalindromes i l h = let ps = createPalindromes l h
                        in case i of
                          Inc -> ps
                          Dec -> reverse ps

_makePalindrome :: Length -> Integer -> Integer
_makePalindrome l = read . makePalindrome l . show
-- _makePalindrome x = read (show x ++ (reverse . show) x)
-- makePalindrome :: Integer -> Integer -> Integer
-- makePalindrome nDigits = go
--   where go x = show

data Length = Short | Long deriving (Show)
toggle :: Length -> Length
toggle Short = Long
toggle Long = Short

makePalindrome :: Length -> [a] -> [a]
makePalindrome l xs = let xs' = reverse xs
                          xs'' = case l of Short -> drop 1 xs'
                                           Long  -> xs'
                    in xs ++ xs''

half :: [a] -> ([a], Length)
half xs = let l = length xs
              n = l `div` 2
          in if odd l
             then (take (n+1) xs, Short)
             else (take n     xs, Long)

_factorsInRange :: Integer -> Integer -> Integer -> [(Integer, Integer)]
_factorsInRange minFactor maxFactor = filter inRange . _factorPairs
  where inRange = both (inBounds minFactor maxFactor)

_factorPairs :: Integer -> [(Integer, Integer)]
_factorPairs x = map (fBoth product . splitList l) . subsequences $ l
  where splitList l xs = (xs, l \\ xs)
        l = primeFactors x

fBoth :: (a -> b) -> (a, a) -> (b, b)
fBoth f (x, y) = (f x, f y)

isqrt :: (Integral c, Integral a) => a -> c
isqrt x = floor . sqrt $ (fromIntegral x :: Float)

type PalindromeIngredients = (Length, Integer)

nextPalindromeIngredients ::
  PalindromeIngredients -> PalindromeIngredients
nextPalindromeIngredients (l, i) =
  let i' = i+1
      overflow = powerOf base i'
      i'' = if overflow
            then case l of
                   Short -> i' `div` base
                   Long  -> i'
        else i'
      l' = if overflow then toggle l else l
      i''length = ishow i''
  in (l', i'')
  where ishow = length . show
        base = 10

powerOf :: Integer -> Integer -> Bool
powerOf b x = let logx = logBase b' x'
              in x == b ^ (round logx)
  where b' = fromIntegral b
        x' = fromIntegral x

createPalindromes :: Integer -> Integer -> [Integer]
createPalindromes minFactor maxFactor =
  let (l, i) = palindromeIngredient (minFactor * minFactor)
  in takeWhile (< maxProduct) . map (uncurry _makePalindrome) $
     iterate nextPalindromeIngredients (l, i)
  where minProduct = square minFactor
        maxProduct = square maxFactor
        square x = x * x

palindromeIngredient :: Integer -> (Length, Integer)
palindromeIngredient i = let (ds, l) = half $ show i
                             d = read ds
                         in (l, d)
