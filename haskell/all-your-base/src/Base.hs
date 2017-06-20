module Base (rebase) where

import Control.Monad (liftM, (>=>))

rebase :: Integral a => a -> a -> [a] -> Maybe [a]
rebase inputBase outputBase
  | outputBase <= 1 = const Nothing
  | inputBase  <= 1 = const Nothing
  | otherwise = unDigits inputBase >=> digits outputBase

unDigits :: Integral a => a -> [a] -> Maybe a
unDigits base = foldl acc (Just 0)
  where acc ds x
          | x >= base = Nothing
          | x < 0     = Nothing
          | otherwise = (x +) . (base *) <$> ds

digits :: Integral a => a -> a -> Maybe [a]
digits base = liftM reverse . digits'
  where digits' 0 = Just []
        digits' n = let (q, r) = n `quotRem` base
                    in (r :) <$> digits' q
