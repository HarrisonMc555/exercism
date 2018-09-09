module Bowling (score, BowlingError(..)) where

data BowlingError = IncompleteGame
                  | InvalidRoll { rollIndex :: Int, rollValue :: Int }
  deriving (Eq, Show)

data BowlingFrame = Strike | Spare | Regular { pins :: Int }

score :: [Int] -> Either BowlingError Int
score = go (Right 0) 0
  where go l@(Left _) _ _ = l
        go (Right score) frameNum rolls =
          | null rolls = Left IncompleteGame
          | roll > maxPins = Left InvalidRoll { rollIndex = frameNum,
                                                rollValue = roll }
          | roll == maxPins = go (Right (score + roll)) nextFrameNum nextRolls
          | roll < maxPins = go 
          where roll = head rolls
                nextRolls = tail rolls
                nextFrameNum = frameNum + 1
          

numFrames :: Int
numFrames = 10

maxPins :: Int
maxPins = 10
