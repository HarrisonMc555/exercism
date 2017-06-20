module House (rhyme) where

import Data.List (intercalate, tails)

type Subject  = String
type Action   = String
type LineInfo = (Subject, Action)

lineInfos :: [LineInfo]
lineInfos = [ ("house that Jack built", undefined)
            , ("malt", "lay in")
            , ("rat", "ate")
            , ("cat", "killed")
            , ("dog", "worried")
            , ("cow with the crumpled horn", "tossed")
            , ("maiden all forlorn", "milked")
            , ("man all tattered and torn", "kissed")
            , ("priest all shaven and shorn", "married")
            , ("rooster that crowed in the morn", "woke")
            , ("farmer sowing his corn", "kept")
            , ("horse and the hound and the horn", "belonged to")
            ]

mkFirstLine :: LineInfo -> String
mkFirstLine (subj, _) = "This is the " ++ subj

mkMiddleLine :: LineInfo -> LineInfo -> String
mkMiddleLine (_, action) (subj, _) = "that " ++ action ++ " the " ++ subj

mkVerse :: [LineInfo] -> String
mkVerse [] = ""
mkVerse vs@(v:_) = intercalate "\n" (firstLine:restLines) ++ ".\n"
  where firstLine = mkFirstLine v
        restLines = map (uncurry mkMiddleLine) (adjacentElems2 vs)

mkVerses :: [LineInfo] -> String
mkVerses vs = intercalate "\n" (map mkVerse vs')
  where vs' = drop 1 . reverse . tails . reverse $ vs

rhyme :: String
rhyme = mkVerses lineInfos

adjacentElems2 :: [a] -> [(a, a)]
adjacentElems2 (x1:x2:xs) = (x1, x2) : adjacentElems2 (x2:xs)
adjacentElems2 _ = []
