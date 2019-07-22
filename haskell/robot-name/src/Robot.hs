module Robot (Robot, mkRobot, resetName, robotName) where

import Data.IORef
import System.Random
import Control.Monad.Random

newtype RobotImpl = RobotImpl { name :: String } deriving (Show, Eq)
type Robot = IORef RobotImpl

mkRobot :: IO Robot
mkRobot = do
  s <- createName
  newIORef (RobotImpl s)

resetName :: Robot -> IO ()
resetName robot = do
  s <- createName
  modifyIORef robot (\_ -> RobotImpl s)

robotName :: Robot -> IO String
robotName r = do
  rval <- readIORef r
  return $ name rval

createName :: IO String
createName = evalRandIO randomName

randomName :: RandomGen g => Rand g String
randomName = liftM2 (++) (letters 2) (digits 3)

letter :: RandomGen g => Rand g Char
letter = getRandomR ('A','Z')

letters :: RandomGen g => Int -> Rand g String
letters = randomSequence letter

digit :: RandomGen g => Rand g Char
digit = getRandomR ('0','9')

digits :: RandomGen g => Int -> Rand g String
digits = randomSequence digit

randomSequence :: (RandomGen g) => Rand g a -> Int -> Rand g [a]
randomSequence r n = replicateM n r
