module Robot (Robot, initialState, mkRobot, resetName, robotName) where

import Control.Arrow ((>>>))
import Control.Concurrent.MVar (MVar, newMVar, putMVar, readMVar, takeMVar)
import Control.Monad.State (StateT, modify, get)
import Control.Monad.Trans (lift)
import Data.Set (Set)
import qualified Data.Set as Set
import System.Random (randomRIO)

newtype Robot = Robot { robotNameVar :: MVar String }
type RunState = Set String

initialState :: RunState
initialState = Set.empty

randomUniqueName :: StateT RunState IO String
randomUniqueName = do
  name <- lift createName
  takenNames <- get
  if Set.member name takenNames
    then randomUniqueName
    else do
    modify $ Set.insert name
    return name

mkRobot :: StateT RunState IO Robot
mkRobot = fmap Robot $ randomUniqueName >>= lift . newMVar

resetName :: Robot -> StateT RunState IO ()
resetName (Robot name) = do
  oldName <- lift $ takeMVar name
  randomUniqueName >>= lift . putMVar name
  modify $ Set.delete oldName
  

robotName :: Robot -> IO String
robotName = robotNameVar >>> readMVar

createName :: IO String
createName = mapM randomRIO [letter, letter, digit, digit, digit]
  where letter = ('A', 'Z')
        digit = ('0', '9')
