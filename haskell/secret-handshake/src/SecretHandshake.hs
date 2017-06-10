module SecretHandshake (handshake) where

import Data.Bits (testBit)

handshake :: Int -> [String]
handshake n = reverse $ foldl f [] secrets
  where f actions (bit, addAction) = applyIf addAction actions $ testBit n bit

secrets :: [(Int, [String] -> [String])]
secrets = [ (0, ("wink" :))
          , (1, ("double blink" :))
          , (2, ("close your eyes" :))
          , (3, ("jump" :))
          , (4, reverse)
          ]

applyIf :: (a -> a) -> a -> Bool -> a
applyIf f x b
  | b         = f x
  | otherwise = x
