module Bowling (score, BowlingError(..)) where

import Data.Maybe (fromMaybe)

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
  do rolls' <- rolls nums
     frames' <- frames rolls'
     let framePairs = unconses frames'
     scores <- mapM (uncurry scoreFrame) $ framePairs
     let score' = sum scores
     return score'

scoreFrame :: Frame -> [Frame] -> Either BowlingError Int
scoreFrame (NormalFrame r1 r2) [] = Left IncompleteGame
scoreFrame (NormalFrame r1 r2) (f:_)
  | isSpare r1 r2 = Right $ maxValue + value (head . frameToRolls $ f)
  | otherwise = Right $ value r1 + value r2
scoreFrame (StrikeFrame r1) [] = Left IncompleteGame
scoreFrame (StrikeFrame r1) (f:rest) =
  let rolls = frameToRolls f ++ concatMap frameToRolls rest
  in case rolls of
    (r2:r3:_) -> Right $ maxValue + value r2 + value r3
    _ -> Left IncompleteGame
scoreFrame (FinalFrame r1 r2 mr3) [] =
  Right $ value r1 + value r2 + maybe 0 value mr3
scoreFrame (FinalFrame _ _ _) (f:_) =
  Left . invalidRoll . head . frameToRolls $ f

frameToRolls :: Frame -> [Roll]
frameToRolls (NormalFrame r1 r2) = [r1, r2]
frameToRolls (StrikeFrame r1) = [r1]
frameToRolls (FinalFrame r1 r2 (Just r3)) = [r1, r2, r3]
frameToRolls (FinalFrame r1 r2 Nothing) = [r1, r2]

rolls :: [Int] -> Either BowlingError [Roll]
rolls = sequence . zipWith roll [0..]

roll :: Int -> Int -> Either BowlingError Roll
roll index value =
  let err = InvalidRoll index value
      guard' = guard err
  in do guard' $ value >= 0
        guard' $ value <= maxValue
        return $ Roll index value

frames :: [Roll] -> Either BowlingError [Frame]
frames rolls =
  do (acc, rest) <- go 0 rolls
     case rest of
       (r:_) -> Left $ invalidRoll r
       [] -> Right $ acc
  where go frameIndex rolls
          | frameIndex < numFrames - 1 =
            do (f, rest) <- frame rolls
               (acc, rest') <- go (frameIndex + 1) rest
               return (f:acc, rest')
          | otherwise = do
              ff <- finalFrame rolls
              return ([ff], [])

rolls' :: [Int] -> [Roll]
rolls' = zipWith Roll [0..]

frame :: [Roll] -> Either BowlingError (Frame, [Roll])
frame [] = Left IncompleteGame
frame (_:[]) = Left IncompleteGame
frame (r1:r2:rest)
  | isStrike r1 = Right (StrikeFrame r1, (r2:rest))
  | otherwise = guard (invalidRoll r2) (isValidFrame r1 r2) >>
                Right (NormalFrame r1 r2, rest)

finalFrame :: [Roll] -> Either BowlingError Frame
finalFrame [] =
  Left IncompleteGame
finalFrame [r1] =
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

isValidFrame, isInvalidFrame :: Roll -> Roll -> Bool
isValidFrame (Roll _ v1) (Roll _ v2) = v1 + v2 <= maxValue
isInvalidFrame = \r -> not . isValidFrame r

unconses :: [a] -> [(a, [a])]
unconses [] = []
unconses all@(x:xs) = (x, xs) : unconses xs

guard :: a -> Bool -> Either a ()
guard err condition = if condition then Right () else Left err

-- frames :: [[Roll]] -> Either BowlingError [Frame]
-- frames groups =
--   let (regular, final) = splitAt (numFrames - 1) groups
--       final' = concat final
--       numFinal = length final'
--   in go
--   where go
--         | numFinal <
--   in case $ length final' of
--        2 -> 
--   in if not $ validFinalFrameLength final'
--      then Left IncompleteGame
--      else do regularFrames <- mapM frame regular
--              finalFrame <- finalFrame final'
--              Right $ regularFrames ++ [finalFrame]

-- frame :: [Roll] -> Either BowlingError Frame
-- frame [roll1]
--   | value roll1 == maxValue = Right $ StrikeFrame roll1
--   | otherwise = Left $ InvalidRoll (index roll1) (value roll1)
-- frame [roll1, roll2]
--   | value roll1 >= maxValue = Left $ invalidRoll roll1
--   | value roll1 + value roll2 > maxValue = Left $ invalidRoll roll2
--   | otherwise = Right $ NormalFrame roll1 roll2
-- frame _ = Left IncompleteGame

-- finalFrame :: [Roll] -> Either BowlingError Frame
-- finalFrame [r1, r2] = Right $ FinalFrame r1 r2 Nothing
-- finalFrame [r1, r2, r3] = Right $ FinalFrame r1 r2 (Just r3)
-- finalFrame _ = Left IncompleteGame

-- invalidRoll :: Roll -> BowlingError
-- invalidRoll (Roll i v) = InvalidRoll i v

-- validFinalFrameLength :: [Roll] -> Bool
-- validFinalFrameLength [_, _] = True
-- validFinalFrameLength [_, _, _] = True
-- validFinalFrameLength _ = False

-- groupRolls :: [Roll] -> [[Roll]]
-- groupRolls = go Nothing
--   where go Nothing [] = []
--         go (Just prev) [] = [[prev]]
--         go Nothing (r:rs) =
--           if value r == maxValue
--           then [r] : go Nothing rs
--           else go (Just r) rs
--         go (Just prev) (r:rs) =
--           [prev, r] : go Nothing rs

-- -- frames :: [Int] -> [[Int]]
-- -- frames = go Nothing
-- --   where go :: Maybe Int -> [Int] -> [[Int]]
-- --         go Nothing [] = []
-- --         go (Just prev) [] = [[prev]]
-- --         go Nothing (roll:rolls) =
-- --           if roll == maxValue
-- --           then [roll] : go Nothing rolls
-- --           else go (Just roll) rolls
-- --         go (Just prev) (roll:rolls) =
-- --           [prev, roll] : go Nothing rolls

-- -- scoreFrame :: Modifier -> [Int] -> Either BowlingError (Int, Modifier)
-- -- scoreFrame _ [] -> BowlingError

-- rolls :: [Int] -> [Roll]
-- rolls = zipWith Roll [0..]
