{-# LANGUAGE OverloadedStrings #-}

module Forth
  ( ForthError(..)
  , ForthState
  , evalText
  , toList
  , emptyState
  ) where

import           Control.Monad  (foldM)
import qualified Data.Char      as Char
import           Data.Foldable  (foldrM)
import           Data.Map       (Map)
import qualified Data.Map       as Map
import qualified Data.Maybe     as Maybe
import           Data.Text      (Text)
import qualified Data.Text      as Text
import qualified Data.Text.Read as Read

data ForthError
  = DivisionByZero
  | StackUnderflow
  | InvalidWord
  | UnknownWord Text
  | SyntaxError
  deriving (Show, Eq)

data ForthState =
  ForthState
    { stack  :: [Int]
    , custom :: Map Text [ForthCommand]
    , mode   :: ForthMode
    }

data ForthToken
  = Number Int
  | Operator (Int -> Int -> Either ForthError Int)
  | BeginCustom
  | EndCustom
  | Custom Text

data ForthCommand
  = CNumber Int
  | COperator (Int -> Int -> Either ForthError Int)
  | Builtin ([Int] -> Either ForthError [Int])

data ForthMode
  = Normal
  | StartingCustomWord
  | DefiningCustomWord Text [ForthCommand]

emptyState :: ForthState
emptyState =
  ForthState {stack = [], custom = initialCustomCommands, mode = Normal}

evalText :: Text -> ForthState -> Either ForthError ForthState
evalText text state = do
  let textWords = Text.words text
  foldM parseAndEvalCommand state textWords

toList :: ForthState -> [Int]
toList = reverse . stack

parseAndEvalCommand :: ForthState -> Text -> Either ForthError ForthState
parseAndEvalCommand state text = do
  token <- parseToken text
  evalToken state token

evalToken :: ForthState -> ForthToken -> Either ForthError ForthState
evalToken state@(ForthState stack' custom' mode') token =
  case mode' of
    StartingCustomWord ->
      case token of
        Custom text ->
          case parseWord text of
            Just name ->
              Right $ ForthState stack' custom' $ DefiningCustomWord name []
            Nothing -> Left InvalidWord
        _ -> Left InvalidWord
    DefiningCustomWord name commands ->
      case token of
        BeginCustom -> Left InvalidWord
        EndCustom ->
          Right $ ForthState stack' (Map.insert name commands custom') Normal
        _ -> do
          command <- tokenToCommand custom' token
          Right $
            ForthState stack' custom' $
            DefiningCustomWord name (command ++ commands)
    Normal ->
      case token of
        BeginCustom -> Right $ ForthState stack' custom' StartingCustomWord
        EndCustom -> Left SyntaxError
        Custom name ->
          case Map.lookup (Text.toLower name) custom' of
            Just commands -> foldrM (flip evalCommand) state commands
            Nothing       -> Left $ UnknownWord name
        _ -> do
          commands <- tokenToCommand custom' token
          foldM evalCommand state commands

evalCommand :: ForthState -> ForthCommand -> Either ForthError ForthState
evalCommand (ForthState stack' custom' mode') command =
  case command of
    CNumber x -> Right $ ForthState (x : stack') custom' mode'
    COperator f ->
      case stack' of
        (x1:x2:rest) -> do
          y <- f x1 x2
          Right $ ForthState (y : rest) custom' mode'
        _ -> Left StackUnderflow
    Builtin f -> do
      stack'' <- f stack'
      Right $ ForthState stack'' custom' mode'

parseToken :: Text -> Either ForthError ForthToken
parseToken text =
  case tryAll text [parseNumber, parseCustomSigils] of
    Just token -> Right token
    Nothing    -> Right $ Custom text

parseNumber :: Text -> Maybe ForthToken
parseNumber text =
  case Read.decimal text of
    Right (x, rest)
      | Text.null rest -> Just $ Number x
    _ -> Nothing

parseCustomSigils :: Text -> Maybe ForthToken
parseCustomSigils text =
  case text of
    ":" -> Just BeginCustom
    ";" -> Just EndCustom
    _   -> Nothing

tokenToCommand ::
     Map Text [ForthCommand] -> ForthToken -> Either ForthError [ForthCommand]
tokenToCommand custom' token =
  case token of
    Number x -> Right [CNumber x]
    Operator f -> Right [COperator f]
    BeginCustom -> Left SyntaxError
    EndCustom -> Left SyntaxError
    Custom name ->
      case Map.lookup (Text.toLower name) custom' of
        Just commands -> Right commands
        Nothing       -> Left $ UnknownWord name

tryAll :: a -> [a -> Maybe b] -> Maybe b
tryAll x = findFirst . map ($ x)

findFirst :: [Maybe a] -> Maybe a
findFirst []           = Nothing
findFirst (Just x:_)   = Just x
findFirst (Nothing:xs) = findFirst xs

parseWord :: Text -> Maybe Text
parseWord text
  | Text.all Char.isDigit text = Nothing
     -- | Maybe.isJust $ parseOperator text = Nothing
  | Maybe.isJust $ parseCustomSigils text = Nothing
  | otherwise = Just . Text.toLower $ text

initialCustomCommands :: Map Text [ForthCommand]
initialCustomCommands = Map.fromList . map (uncurry convert) $ builtins
  where
    convert name f = (name, [Builtin f])

builtins :: [(Text, [Int] -> Either ForthError [Int])]
builtins =
  [ ("+", add)
  , ("-", sub)
  , ("*", mul)
  , ("/", div')
  , ("dup", dup)
  , ("swap", swap)
  , ("drop", drop')
  , ("over", over)
  ]
  where
    add = wrap (+)
    sub = wrap (-)
    mul = wrap (*)
    div' (x1:x2:rest) =
      if x1 == 0
        then Left DivisionByZero
        else Right $ x2 `div` x1 : rest
    div' _ = Left StackUnderflow
    dup (x:rest) = Right $ x : x : rest
    dup _        = Left StackUnderflow
    swap (x1:x2:rest) = Right $ x2 : x1 : rest
    swap _            = Left StackUnderflow
    drop' (_:rest) = Right rest
    drop' _        = Left StackUnderflow
    over (x1:x2:rest) = Right $ x2 : x1 : x2 : rest
    over _            = Left StackUnderflow
    wrap f (x1:x2:rest) = Right $ f x2 x1 : rest
    wrap _ _            = Left StackUnderflow
