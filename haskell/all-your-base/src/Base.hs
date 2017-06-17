module Base (rebase) where

import Control.Monad (liftM)

rebase :: Integral a => a -> a -> [a] -> Maybe [a]
rebase inputBase outputBase ds
  | outputBase <= 1 = Nothing
  | inputBase  <= 1 = Nothing
  | otherwise = liftM reverse rebased
    where rebased = unDigits inputBase ds' >>= digits outputBase
          ds' = reverse ds

unDigits :: Integral a => a -> [a] -> Maybe a
unDigits _ [] = Just 0
unDigits inputBase (d:ds)
  | d >= inputBase = Nothing
  | d <  0         = Nothing
  | otherwise      = (d +) . (inputBase *) <$> unDigits inputBase ds

digits :: Integral a => a -> a -> Maybe [a]
digits _ 0 = Just []
digits b n = (r :) <$> digits b q
  where (q, r) = n `quotRem` b
