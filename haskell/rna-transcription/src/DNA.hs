module DNA (toRNA) where

toRNA :: String -> Maybe String
toRNA ""     = Just ""
toRNA (x:xs) = fmap (:) (baseToRNA x) <*> toRNA xs

baseToRNA :: Char -> Maybe Char
baseToRNA c = case c of
                'G' -> Just 'C'
                'C' -> Just 'G'
                'T' -> Just 'A'
                'A' -> Just 'U'
                _   -> Nothing
