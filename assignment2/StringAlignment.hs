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
    
    -- ANVÄNDER SAMMA LOOKUP-TABELL SOM I MCS-PROBLEMET --
    similarityScore :: String -> String -> Int
    similarityScore xs ys = getEntry (length xs) (length ys)
        where
            getEntry i j = table !! i !! j

            table :: [[Int]]
            table = [[entry i j | j <- [0..]] | i <- [0..]]

            entry :: Int -> Int -> Int
            entry _ 0 = 0
            entry 0 _ = 0
            entry i j = charScore x y + max (getEntry i (j - 1)) (getEntry (i - 1) j)
                where
                    x = xs !! (i - 1)
                    y = ys !! (j - 1)

    attachHeads :: a -> a -> [([a],[a])] -> [([a],[a])]
    attachHeads h1 h2 aList = [(h1:xs,h2:ys) | (xs,ys) <- aList]

    maximaBy :: Ord b => (a -> b) -> [a] -> [a]
    maximaBy f xs = filter (\x -> f x == m) xs
        where m = maximum $ map f xs

    type AlignmentType = (String,String)

    charScore :: Char -> Char -> Int
    charScore x y
        | x == '-' = scoreSpace
        | y == '-' = scoreSpace
        | x == y = scoreMatch
        | otherwise = scoreMismatch

    score :: String -> String -> Int
    score string [] = (*scoreSpace) $ length string
    score [] string = (*scoreSpace) $ length string
    score (x:xs) (y:ys) = charScore x y + score xs ys

    optAlignments :: String -> String -> [AlignmentType]
    optAlignments [] [] = [("","")]
    optAlignments (x:xs) [] = attachHeads x '-' $ optAlignments xs []
    optAlignments [] (y:ys) = attachHeads '-' y $ optAlignments [] ys
    optAlignments (x:xs) (y:ys) = maximaBy (uncurry score) $ xy ++ x_ ++ _y
        where   xy = (attachHeads x      y   (optAlignments xs       ys      ))
                x_ = (attachHeads x      '-' (optAlignments xs       (y:ys)  ))
                _y = (attachHeads '-'    y   (optAlignments (x:xs)   ys      ))

    outputOptAlignments :: String -> String -> IO ()
    outputOptAlignments s1 s2 = do
        putStr "\n"
        putStrLn $ "There are " ++ (show . length) res ++ " optimal alignments!"
        recPrint res
        where
            res = optAlignments s1 s2
            recPrint :: [AlignmentType] -> IO ()
            recPrint [] = do putStrLn ""
            recPrint ((a,b):xs) = do
                putStr "\n"
                putStrLn $ spaceify a
                putStrLn $ spaceify b
                recPrint xs
                where
                    spaceify :: String -> String
                    spaceify [] = []
                    spaceify (c:cs) = c : ' ' : spaceify cs 
