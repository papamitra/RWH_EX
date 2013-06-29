
-- ex2
splitWith :: (a -> Bool) -> [a] -> [[a]]
splitWith pred xs =
  case span pred xs of
    ([], []) -> []
    ([], suf) -> drp suf
    (pre,suf) -> pre : (drp suf)
  where drp xs = let (pre, suf) = break pred xs
                 in splitWith pred suf

