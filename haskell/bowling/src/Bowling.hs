module Bowling (score, BowlingError(..)) where

import Control.Monad (zipWithM)

data BowlingError = IncompleteGame
                  | InvalidRoll { rollIndex :: Int, rollValue :: Int }
  deriving (Eq, Show)

data Modifier = Normal
              | Spare
              | Strike
  deriving (Eq, Show)

data Frame = NormalFrame Roll Roll
           | StrikeFrame Roll
           | FinalFrame Roll Roll (Maybe Roll)
  deriving (Eq, Show)

data Roll = Roll { index :: Int
                 , value :: Int
                 }
  deriving (Eq, Show)

numFrames :: Int
numFrames = 10

maxValue :: Int
maxValue = 10

score :: [Int] -> Either BowlingError Int
score nums =
  do rolls' <- getRolls nums
     frames' <- frames rolls'
     let framePairs = unconses frames'
     scores <- mapM (uncurry scoreFrame) framePairs
     let score' = sum scores
     return score'

scoreFrame :: Frame -> [Frame] -> Either BowlingError Int
scoreFrame (NormalFrame _ _) [] = Left IncompleteGame
scoreFrame (NormalFrame r1 r2) (f:_)
  | isSpare r1 r2 = Right $ maxValue + value (head . frameToRolls $ f)
  | otherwise = Right $ value r1 + value r2
scoreFrame (StrikeFrame _) [] = Left IncompleteGame
scoreFrame (StrikeFrame _) (f:rest) =
  let rolls = frameToRolls f ++ concatMap frameToRolls rest
  in case rolls of
    (r2:r3:_) -> Right $ maxValue + value r2 + value r3
    _ -> Left IncompleteGame
scoreFrame (FinalFrame r1 r2 mr3) [] =
  Right $ value r1 + value r2 + maybe 0 value mr3
scoreFrame (FinalFrame {}) (f:_) =
  Left . invalidRoll . head . frameToRolls $ f

frameToRolls :: Frame -> [Roll]
frameToRolls (NormalFrame r1 r2) = [r1, r2]
frameToRolls (StrikeFrame r1) = [r1]
frameToRolls (FinalFrame r1 r2 (Just r3)) = [r1, r2, r3]
frameToRolls (FinalFrame r1 r2 Nothing) = [r1, r2]

getRolls :: [Int] -> Either BowlingError [Roll]
getRolls = zipWithM roll [0..]

roll :: Int -> Int -> Either BowlingError Roll
roll index' value' =
  let err = InvalidRoll index' value'
      guard' = guard err
  in do guard' $ value' >= 0
        guard' $ value' <= maxValue
        return $ Roll index' value'

frames :: [Roll] -> Either BowlingError [Frame]
frames rolls =
  do (acc, rest) <- go 0 rolls
     case rest of
       (r:_) -> Left $ invalidRoll r
       [] -> Right acc
  where go frameIndex rolls'
          | frameIndex < numFrames - 1 =
            do (f, rest) <- frame rolls'
               (acc, rest') <- go (frameIndex + 1) rest
               return (f:acc, rest')
          | otherwise = do
              ff <- finalFrame rolls'
              return ([ff], [])

frame :: [Roll] -> Either BowlingError (Frame, [Roll])
frame [] = Left IncompleteGame
frame [_] = Left IncompleteGame
frame (r1:r2:rest)
  | isStrike r1 = Right (StrikeFrame r1, r2:rest)
  | otherwise = guard (invalidRoll r2) (isValidFrame r1 r2) >>
                Right (NormalFrame r1 r2, rest)

finalFrame :: [Roll] -> Either BowlingError Frame
finalFrame [] =
  Left IncompleteGame
finalFrame [_] =
  Left IncompleteGame
finalFrame [r1, r2] =
  do guard IncompleteGame $ not $ isStrike r1
     guard IncompleteGame $ not $ isSpare r1 r2
     guard (invalidRoll r2) $ isValidFrame r1 r2
     return $ FinalFrame r1 r2 Nothing
finalFrame [r1, r2, r3] =
  let right = Right ()
      guarded = if isStrike r1
                then if isStrike r2
                     then right
                     else guard (invalidRoll r3) (isValidFrame r2 r3)
                else guard (invalidRoll r3) (isSpare r1 r2) >>
                     guard (invalidRoll r2) (isValidFrame r1 r2)
  in guarded >> Right (FinalFrame r1 r2 (Just r3))
finalFrame (r1:r2:r3:r4:_) =
  if isStrike r1 || isSpare r1 r2
  then Left . invalidRoll $ r4
  else Left . invalidRoll $ r3

invalidRoll :: Roll -> BowlingError
invalidRoll (Roll i v) = InvalidRoll i v

isStrike :: Roll -> Bool
isStrike (Roll _ v) = v == maxValue

isSpare :: Roll -> Roll -> Bool
isSpare (Roll _ v1) (Roll _ v2) = v1 + v2 == maxValue

isValidFrame :: Roll -> Roll -> Bool
isValidFrame (Roll _ v1) (Roll _ v2) = v1 + v2 <= maxValue

unconses :: [a] -> [(a, [a])]
unconses [] = []
unconses (x:xs) = (x, xs) : unconses xs

guard :: a -> Bool -> Either a ()
guard err condition = if condition then Right () else Left err
