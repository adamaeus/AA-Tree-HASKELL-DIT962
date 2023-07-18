{-
  to run:
  $ ghc -e main Main.hs < swahili-small.txt

  to compile and run:
  $ ghc -O Main.hs && ./Main < swahili-small.txt
-}

import AATree

--------------------------------------------------------------------------------
-- Function calls to generate trees from words in external files
main :: IO ()
main = do
  contents <- getContents

  let list = words contents

  let tree = foldl (flip insert) emptyTree list

  let s = size tree
  let h = bstHeight tree
  let c = checkTree tree
  let oh = ceiling (logBase 2 (fromIntegral (s + 1))) - 1
  let r = fromIntegral h / fromIntegral oh
  let fw = take 20 (inorder tree)


  -- calculate and print statistics
  putStrLn $ unlines
              [ "Size: " ++ show s
              , "Height: " ++ show h
              , "Optimal height: " ++ show oh
              , "Height / Optimal height: " ++ show r
              , "checkTree: " ++ show c
              , "First 20 words: " ++ show fw
              ]
--------------------------------------------------------------------------------
