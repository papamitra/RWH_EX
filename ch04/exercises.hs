
-- Problem 1
safeHead :: [a] -> Maybe a
safeHead [] = Nothing
safeHead xs = Just $ head xs

safeLast :: [a] -> Maybe a
safeLast [] = Nothing
safeLast xs = Just $ last xs
