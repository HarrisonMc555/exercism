module Base (rebase, Error(..)) where

import Control.Monad ((>=>))

data Error a = InvalidInputBase
             | InvalidOutputBase
             | InvalidDigit a
             deriving (Show, Eq)

rebase :: Integral a => a -> a -> [a] -> Either (Error a) [a]
rebase inputBase outputBase
  | inputBase  <= 1 = const (Left InvalidInputBase)
  | outputBase <= 1 = const (Left InvalidOutputBase)
  | otherwise = unDigits inputBase >=> digits outputBase >=> Right

unDigits :: Integral a => a -> [a] -> Either (Error a) a
unDigits base = foldl acc (Right 0)
  where acc ds x
          | x >= base = Left (InvalidDigit x)
          | x < 0     = Left (InvalidDigit x)
          | otherwise = (x +) . (base *) <$> ds

digits :: Integral a => a -> a -> Either (Error a) [a]
digits base = fmap reverse . digits'
  where digits' 0 = Right []
        digits' n = let (q, r) = n `quotRem` base
                    in (r :) <$> digits' q
