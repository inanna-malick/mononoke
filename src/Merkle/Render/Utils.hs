module Merkle.Render.Utils where

-- | indent and concat
-- [["a", "b"], ["c"], ["d"], []] results in
--
-- ├── a
-- │   b
-- ├── c
-- └── d
indent :: [[String]] -> [String]
indent segments =
  let apply _ _ [] = []
      apply f g (x:xs) = f x : fmap g xs
      removeEmpty = filter (not . null)
    in mconcat $ reverse $ apply
         (apply ("└── " ++) ("    " ++))
         (apply ("├── " ++) ("│   " ++))
         (reverse $ removeEmpty segments)

