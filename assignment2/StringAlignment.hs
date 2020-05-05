module StringAlignment where

	scoreMatch = 0
	scoreMismatch = -1
	scoreSpace = -1

	-- STRÄNGAR --

	string1 = "writers"
	string2 = "vintner"
	string3 = "aferouciousmonadatemyhamster"
	string4 = "functionalprogrammingrules"
	string5 = "bananrepubliksinvasionsarmestabsadjutant"
	string6 = "kontrabasfiolfodralmakarmästarlärling"

    similarityScore :: String -> String -> Int
    similarityScore s1 s2 = 0

    attachHeads :: a -> a -> [([a],[a])] -> [([a],[a])]
    attachHeads h1 h2 aList = [(h1:xs,h2:ys) | (xs,ys) <- aList]
    

    -- ANVÄNDER SAMMA LOOKUP-TABELL SOM I MCS-PROBLEMET --
    similarityScore :: String -> String -> Int
    similarityScore xs ys = getIndex (length xs) (length ys)
      where
    	getEntry i j = table !! i !! j

    	table :: [[Int]]
    	table = [[entry i j | j <- [0..]] | i <- [0..]]

    	entry :: Int -> Int -> Int
    	entry _ 0 = 0
    	entry 0 _ = 0
    	entry i j
    	  | x = y = 1 + getEntry (i - 1) (j - 1)
    	  | otherwise = max (getEntry i (j - 1)) (getEntry (i - 1) j)
    	  where
    	  	x = xs !! (i - 1)
    	  	y = ys !! (j - 1)


