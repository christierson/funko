module StringAlignment where

    similarityScore :: String -> String -> Int
    similarityScore s1 s2 = 0

    attachHeads :: a -> a -> [([a],[a])] -> [([a],[a])]
    attachHeads h1 h2 aList = [(h1:xs,h2:ys) | (xs,ys) <- aList]
    