module DNA (nucleotideCounts) where
import Data.Map (Map, fromList, alter, member)

nucleotides :: String
nucleotides = "AGCT"

emptyNucleotideMap = Data.Map.fromList $ Prelude.map init nucleotides
  where init c = (c, 0)

nucleotideCounts :: String -> Either String (Map Char Int)
nucleotideCounts = nucleotideCountsAccum emptyNucleotideMap
  where nucleotideCountsAccum m "" = Right m
        nucleotideCountsAccum m (c:cs)
          | member c emptyNucleotideMap = nucleotideCountsAccum m' cs
          | otherwise                   = Left $ "Invalid nucleotide" ++ [c]
          where m'  = alter inc c m
                inc = fmap (1+)