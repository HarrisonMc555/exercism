{-# LANGUAGE ExistentialQuantification, MultiParamTypeClasses #-}
module ZebraPuzzle (Resident(..), Solution(..), solve) where

import Control.Applicative ((<|>))
import Data.Maybe (isJust)
import qualified Data.Set as Set
import Data.Foldable (find)
import Data.Maybe (fromJust)
import Data.List (intercalate)

data Solution = Solution { waterDrinker :: Resident
                         , zebraOwner :: Resident
                         } deriving (Eq, Show)

data Resident = Englishman | Spaniard | Ukrainian | Norwegian | Japanese
  deriving (Eq, Enum, Bounded, Show)

data Color = Red | Yellow | Green | Blue | Ivory
  deriving (Eq, Enum, Bounded, Show)

data Drink = Coffee | Milk | OrangeJuice | Tea | Water
  deriving (Eq, Enum, Bounded, Show)

data Smoke = OldGold | Kools | Chesterfields | LuckyStrike | Parliaments
  deriving (Eq, Enum, Bounded, Show)

data Pet = Dog | Snails | Fox | Horse | Zebra
  deriving (Eq, Enum, Bounded, Show)

data Location = First | Second | Third | Fourth | Fifth
  deriving (Eq, Enum, Bounded, Show)

data Fact = Fact { resident :: Maybe Resident
                 , color    :: Maybe Color
                 , drink    :: Maybe Drink
                 , smoke    :: Maybe Smoke
                 , pet      :: Maybe Pet
                 , location :: Maybe Location
                 } deriving (Eq)
instance Show Fact where
  show (Fact r c d s p l) = "{" ++ intercalate ", " (map showMaybe xs) ++ "}"
    where xs = [SB <$> r, SB <$> c, SB <$> d, SB <$> s, SB <$> p, SB <$> l]

data ShowBox = forall s . Show s => SB s
instance Show ShowBox where
  show (SB s) = show s

class FactItem fi where
  singleItem :: fi -> Fact
  
instance FactItem Resident where
  singleItem r = emptyFact { resident = Just r }
instance FactItem Color where
  singleItem c = emptyFact { color = Just c }
instance FactItem Drink where
  singleItem d = emptyFact { drink = Just d }
instance FactItem Smoke where
  singleItem s = emptyFact { smoke = Just s }
instance FactItem Pet where
  singleItem p = emptyFact { pet = Just p }
instance FactItem Location where
  singleItem l = emptyFact { location = Just l }

type Rule = [Fact] -> Maybe Fact

showMaybe :: Show s => Maybe s -> String
showMaybe = maybe "?" show

solve :: Solution
solve = error "You need to implement this function."

emptyFact :: Fact
emptyFact = Fact Nothing Nothing Nothing Nothing Nothing Nothing

facts :: [Fact]
facts = [ emptyFact { resident = Just Englishman
                    , color    = Just Red
                    }
        , emptyFact { resident = Just Spaniard
                    , pet      = Just Dog
                    }
        , emptyFact { drink    = Just Coffee
                    , color    = Just Green
                    }
        , emptyFact { resident = Just Ukrainian
                    , drink    = Just Tea
                    }
        , emptyFact { smoke    = Just OldGold
                    , pet      = Just Snails
                    }
        , emptyFact { smoke    = Just Kools
                    , color    = Just Yellow
                    }
        , emptyFact { drink    = Just Milk
                    , location = Just Third
                    }
        , emptyFact { resident = Just Norwegian
                    , location = Just First
                    }
        , emptyFact { smoke    = Just LuckyStrike
                    , drink    = Just OrangeJuice
                    }
        , emptyFact { resident = Just Japanese
                    , smoke    = Just Parliaments
                    }
        ]

conflict :: (Eq a) => Maybe a -> Maybe a -> Bool
conflict x y = (x <|> y) /= (y <|> x)

common :: (Eq a) => Maybe a -> Maybe a -> Bool
common x y = x == y && isJust x

compatible :: Fact -> Fact -> Bool
compatible = go
  where go fA fB = not $ factConflict fA fB

factConflict :: Fact -> Fact -> Bool
factConflict fA@(Fact rA cA dA sA pA lA) fB@(Fact rB cB dB sB pB lB)
  = conflict rA rB ||
    conflict cA cB ||
    conflict dA dB ||
    conflict sA sB ||
    conflict pA pB ||
    conflict lA lB

factCompatible :: Fact -> Fact -> Bool
factCompatible = go
  where go f1 f2 = not $ factConflict f1 f2

factCommon :: Fact -> Fact -> Bool
factCommon fA@(Fact rA cA dA sA pA lA) fB@(Fact rB cB dB sB pB lB)
  = common rA rB ||
    common cA cB ||
    common dA dB ||
    common sA sB ||
    common pA pB ||
    common lA lB


combine :: Fact -> Fact -> Either (Fact, Fact) Fact
combine f1@(Fact r1 c1 d1 s1 p1 l1) f2@(Fact r2 c2 d2 s2 p2 l2)
  = if factCommon f1 f2 && not (factConflict f1 f2)
    then Right $ Fact (r1 <|> r2) (c1 <|> c2) (d1 <|> d2)
         (s1 <|> s2) (p1 <|> p2) (l1 <|> l2)
    else Left (f1, f2)

-- nubOrd :: (Ord a) => [a] -> [a]
-- nubOrd xs = go Set.empty xs where
--   go s (x:xs')
--     | x `Set.member` s = go s xs'
--     | otherwise        = x : go (Set.insert x s) xs'
--   go _ _               = []

-- isAll :: (Enum a, Bounded a, Eq a) => [a] -> Bool
-- isAll xs = all (`elem` xs) [minBound.. maxBound]

-- The green house is immediately to the right of the ivory house.
greenIvoryRule :: Foldable t => t Fact -> Maybe Fact
greenIvoryRule fs = let f = find (\f -> color f == Just Ivory) fs
                        l = succ . fromJust . location <$> f
                        addLocation f' l' = f' { location = Just l' }
                    in addLocation emptyFact { color = Just Green } <$> l

-- The man who smokes Chesterfields lives in the house next to the man with the
-- fox.
chesterfieldsRule :: Foldable t => t Fact -> Maybe Fact
chesterfieldsRule fs =
  let f = find (\f -> pet f == Just Fox) fs
      ll = pred . fromJust . location <$> f
      lr = succ . fromJust . location <$> f
      fl = case ll of
        Just ll' -> find (\f -> location f == Just ll') fs
        Nothing -> Nothing
      fr = case lr of
        Just lr' -> find (\f -> location f == Just lr') fs
        Nothing -> Nothing
      addLocation f' l' = f' { location = Just l' }
  in case (fl, fr) of
       (Nothing, Nothing) -> Nothing
       (Just _ , Nothing) ->
         addLocation emptyFact { smoke = Just Chesterfields } <$> lr
       (Nothing, Just _)  ->
         addLocation emptyFact { smoke = Just Chesterfields } <$> ll
       (Just _ , Just _)  -> error "Rule can't be satisfied"

nextToRule :: (FactItem fi1, FactItem fi2, Foldable t) =>
  fi1 ->     -- This item is next to...
  fi2 ->     -- ...this item
  t Fact ->  -- List of facts so far
  Maybe Fact -- Result (Nothing if can't determine anything)
nextToRule i1 i2 fs = let f = findFactItem i2 fs
                          
  in Nothing

findFactItem :: (Foldable t, FactItem fi) => fi -> t Fact -> Maybe Fact
findFactItem fi = find (factCommon (singleItem fi))
  
  

safeSucc, safePred :: (Enum a, Bounded a, Eq a) => a -> Maybe a
safeSucc a
  | a == minBound = Nothing
  | otherwise     = Just (succ a)
safePred a
  | a == maxBound = Nothing
  | otherwise     = Just (pred a)
