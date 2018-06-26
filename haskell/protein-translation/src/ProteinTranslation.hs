module ProteinTranslation(proteins) where

import Data.List (find)
import Data.List.Split (chunksOf)
import Control.Arrow ((>>>))
import Control.Monad (sequence, liftM)

proteins :: String -> Maybe [String]
proteins = rnaToCodons >>> map codonToAminoAcid >>>
           sequence >>> liftM (takeWhile notStop)
  where notStop = (/= stopCodon)

rnaToCodons :: String -> [(Char, Char, Char)]
rnaToCodons = map triplet . chunksOf 3
  where triplet [a, b, c] = (a, b, c)
        triplet _ = error "Input RNA length not multiple of three"

codonToAminoAcid :: (Char, Char, Char) -> Maybe String
codonToAminoAcid (a, b, c) =
  let aminoAcid = snd <$> find containsCodon codonToAminoAcidMap
      containsCodon (cs, _) = codon `elem` cs
      codon = [a, b, c]
  in aminoAcid

codonToAminoAcidMap :: [([String], String)]
codonToAminoAcidMap =
  [
    (["AUG"],
     "Methionine"),
    (["UUU", "UUC"],
     "Phenylalanine"),
    (["UUA", "UUG"],
     "Leucine"),
    (["UCU", "UCC", "UCA", "UCG"],
     "Serine"),
    (["UAU", "UAC"],
     "Tyrosine"),
    (["UGU", "UGC"],
     "Cysteine"),
    (["UGG"],
     "Tryptophan"),
    (["UAA", "UAG", "UGA"],
     stopCodon)
  ]

stopCodon :: String
stopCodon = "STOP"
