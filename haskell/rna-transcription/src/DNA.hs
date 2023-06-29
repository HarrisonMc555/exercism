module DNA (toRNA) where

toRNA :: String -> Either Char String
toRNA = traverse baseToRNA

baseToRNA :: Char -> Either Char Char
baseToRNA c = case c of
                'G' -> Right 'C'
                'C' -> Right 'G'
                'T' -> Right 'A'
                'A' -> Right 'U'
                _   -> Left c