module DNA (nucleotideCounts, Nucleotide(..)) where
import Data.Map (Map, fromList, alter, member)

data Nucleotide = A | G | C | T deriving (Eq, Ord, Show)

nucleotides :: [Nucleotide]
nucleotides = [A, G, C, T]

emptyNucleotideMap :: Map Nucleotide Int
emptyNucleotideMap = Data.Map.fromList $ map empty nucleotides
  where empty c = (c, 0)

nucleotideCounts :: String -> Either String (Map Nucleotide Int)
nucleotideCounts = nucleotideCountsAccum emptyNucleotideMap
  where nucleotideCountsAccum m "" = Right m
        nucleotideCountsAccum m (c:cs) =
          case fromChar c of
            Just n -> let m' = alter (fmap (+1)) n m
                      in nucleotideCountsAccum m' cs
            Nothing -> Left $ "Invalid nucleotide " ++ [c]

fromChar :: Char -> Maybe Nucleotide
fromChar 'A' = Just A
fromChar 'G' = Just G
fromChar 'C' = Just C
fromChar 'T' = Just T
fromChar _ = Nothing
