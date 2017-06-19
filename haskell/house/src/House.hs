module House (rhyme) where

import Data.List (intercalate, tails)

type Subject    = String
type Action     = String
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
mkVerse vs@(v:_) =
  intercalate "\n" (firstLine:restLines) ++ ".\n"
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

-- rhyme :: String
-- rhyme = "This is the house that Jack built.\n\
--         \\n\
--         \This is the malt\n\
--         \that lay in the house that Jack built.\n\
--         \\n\
--         \This is the rat\n\
--         \that ate the malt\n\
--         \that lay in the house that Jack built.\n\
--         \\n\
--         \This is the cat\n\
--         \that killed the rat\n\
--         \that ate the malt\n\
--         \that lay in the house that Jack built.\n\
--         \\n\
--         \This is the dog\n\
--         \that worried the cat\n\
--         \that killed the rat\n\
--         \that ate the malt\n\
--         \that lay in the house that Jack built.\n\
--         \\n\
--         \This is the cow with the crumpled horn\n\
--         \that tossed the dog\n\
--         \that worried the cat\n\
--         \that killed the rat\n\
--         \that ate the malt\n\
--         \that lay in the house that Jack built.\n\
--         \\n\
--         \This is the maiden all forlorn\n\
--         \that milked the cow with the crumpled horn\n\
--         \that tossed the dog\n\
--         \that worried the cat\n\
--         \that killed the rat\n\
--         \that ate the malt\n\
--         \that lay in the house that Jack built.\n\
--         \\n\
--         \This is the man all tattered and torn\n\
--         \that kissed the maiden all forlorn\n\
--         \that milked the cow with the crumpled horn\n\
--         \that tossed the dog\n\
--         \that worried the cat\n\
--         \that killed the rat\n\
--         \that ate the malt\n\
--         \that lay in the house that Jack built.\n\
--         \\n\
--         \This is the priest all shaven and shorn\n\
--         \that married the man all tattered and torn\n\
--         \that kissed the maiden all forlorn\n\
--         \that milked the cow with the crumpled horn\n\
--         \that tossed the dog\n\
--         \that worried the cat\n\
--         \that killed the rat\n\
--         \that ate the malt\n\
--         \that lay in the house that Jack built.\n\
--         \\n\
--         \This is the rooster that crowed in the morn\n\
--         \that woke the priest all shaven and shorn\n\
--         \that married the man all tattered and torn\n\
--         \that kissed the maiden all forlorn\n\
--         \that milked the cow with the crumpled horn\n\
--         \that tossed the dog\n\
--         \that worried the cat\n\
--         \that killed the rat\n\
--         \that ate the malt\n\
--         \that lay in the house that Jack built.\n\
--         \\n\
--         \This is the farmer sowing his corn\n\
--         \that kept the rooster that crowed in the morn\n\
--         \that woke the priest all shaven and shorn\n\
--         \that married the man all tattered and torn\n\
--         \that kissed the maiden all forlorn\n\
--         \that milked the cow with the crumpled horn\n\
--         \that tossed the dog\n\
--         \that worried the cat\n\
--         \that killed the rat\n\
--         \that ate the malt\n\
--         \that lay in the house that Jack built.\n\
--         \\n\
--         \This is the horse and the hound and the horn\n\
--         \that belonged to the farmer sowing his corn\n\
--         \that kept the rooster that crowed in the morn\n\
--         \that woke the priest all shaven and shorn\n\
--         \that married the man all tattered and torn\n\
--         \that kissed the maiden all forlorn\n\
--         \that milked the cow with the crumpled horn\n\
--         \that tossed the dog\n\
--         \that worried the cat\n\
--         \that killed the rat\n\
--         \that ate the malt\n\
--         \that lay in the house that Jack built.\n"
