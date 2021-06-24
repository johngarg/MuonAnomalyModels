module Utils where

concatMaybe :: Maybe String -> Maybe String -> Maybe String
concatMaybe (Just x) (Just y) = Just (x ++ y)
concatMaybe _        _        = Nothing
