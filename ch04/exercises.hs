
-- ex1
safeHead :: [a] -> Maybe a
safeHead [] = Nothing
safeHead xs = Just $ head xs

-- m-oshita
safeHead_m :: [a] -> Maybe a
safeHead_m [] = Nothing
safeHead_m (x:xs) = Just x

safeLast :: [a] -> Maybe a
safeLast [] = Nothing
safeLast xs = Just $ last xs


-- ex2
splitWith :: (a -> Bool) -> [a] -> [[a]]
splitWith pred xs =
  case span pred xs of
    ([], []) -> []
    ([], suf) -> drp suf
    (pre,suf) -> pre : (drp suf)
  where drp xs = let (pre, suf) = break pred xs
                 in splitWith pred suf

-- m-oshita
splitWith_m :: (a -> Bool) -> [a] -> [[a]]
splitWith_m f x = splitWith' f x [] []
  where splitWith' f [] ls rt | null ls = reverse rt
                              | otherwise = reverse $ (reverse ls):rt
        splitWith' f (x:xs) ls rt | (f x) == True = splitWith' f xs (x:ls) rt
                                  | (f x) == False && null ls = splitWith' f xs [] rt 
                                  | otherwise = splitWith' f xs [] ((reverse ls):rt)
