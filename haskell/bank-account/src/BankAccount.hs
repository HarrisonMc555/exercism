module BankAccount
    ( BankAccount
    , closeAccount
    , getBalance
    , incrementBalance
    , openAccount
    ) where

import Data.IORef (IORef, newIORef, readIORef, writeIORef)

newtype BankAccountImpl = BankAccountImpl { balance :: Maybe Integer
                                          } deriving (Show)

type BankAccount = IORef BankAccountImpl

closeAccount :: BankAccount -> IO ()
closeAccount account = writeIORef account closedAccount

getBalance :: BankAccount -> IO (Maybe Integer)
getBalance account = do
  b <- readIORef account
  return (balance b)

incrementBalance :: BankAccount -> Integer -> IO (Maybe Integer)
incrementBalance account amount = do
  b <- readIORef account
  let b' = incrementAccount amount b
  writeIORef account b'
  return (balance b')

openAccount :: IO BankAccount
openAccount = newIORef (newAccount 0)

newAccount :: Integer -> BankAccountImpl
newAccount = BankAccountImpl . Just

closedAccount :: BankAccountImpl
closedAccount = BankAccountImpl Nothing

incrementAccount :: Integer -> BankAccountImpl -> BankAccountImpl
incrementAccount n bi = BankAccountImpl ((+n) <$> balance bi)
