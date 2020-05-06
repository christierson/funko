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

    similarityScore :: String -> String -> Int
    similarityScore [] [] = 0
    similarityScore [] ys = (*) scoreSpace $ length ys
    similarityScore xs [] = (*) scoreSpace $ length xs
    similarityScore (x:xs) (y:ys) = maximum [
        charScore x y + similarityScore xs ys,
        charScore x '-' + similarityScore xs (y:ys),
        charScore '-' y + similarityScore (x:xs) ys]

    similarityScoreOptimized :: String -> String -> Int
    similarityScoreOptimized xs ys = getEntry (length xs) (length ys)
        where
            getEntry i j = table !! i !! j
            
            table :: [[Int]]
            table = [[entry i j | j <- [0..]] | i <- [0..]]

            entry :: Int -> Int -> Int
            entry 0 0 = 0
            entry 0 j = scoreSpace * j
            entry i 0 = scoreSpace * i
            entry i j = maximum [
                charScore x     y   + getEntry (i - 1)  (j - 1),
                charScore x     '-' + getEntry (i - 1)  j,
                charScore '-'   y   + getEntry i        (j - 1)]
                where
                    x = xs !! (i - 1)
                    y = ys !! (j - 1)


    attachLast :: a -> a -> [([a],[a])] -> [([a],[a])]
    attachLast h1 h2 aList = [(xs ++ [h1], ys ++ [h2]) | (xs,ys) <- aList]

    attachHeads :: a -> a -> [([a],[a])] -> [([a],[a])]
    attachHeads h1 h2 aList = [(h1:xs,h2:ys) | (xs,ys) <- aList]

    maximaBy :: Ord b => (a -> b) -> [a] -> [a]
    maximaBy f xs = filter (\x -> f x == m) xs
        where m = maximum $ map f xs

    optAlignments :: String -> String -> [AlignmentType]
    optAlignments [] [] = [("","")]
    optAlignments (x:xs) [] = attachHeads x '-' $ optAlignments xs []
    optAlignments [] (y:ys) = attachHeads '-' y $ optAlignments [] ys
    optAlignments (x:xs) (y:ys) = maximaBy (uncurry score) $ xy ++ x_ ++ _y
        where   xy = (attachHeads x      y   (optAlignments xs       ys      ))
                x_ = (attachHeads x      '-' (optAlignments xs       (y:ys)  ))
                _y = (attachHeads '-'    y   (optAlignments (x:xs)   ys      ))

    type Entry = (Int, [AlignmentType])

    optAlignmentsOptimized :: String -> String -> [AlignmentType]
    optAlignmentsOptimized xs ys = snd $ getEntry (length xs) (length ys)
        where
            getEntry :: Int -> Int -> Entry
            getEntry i j = table !! i !! j

            table :: [[Entry]]
            table = [[entry i j | j <- [0..]] | i <- [0..]]

            addChars :: Char -> Char -> Entry -> Entry
            addChars x y (s, al) = (s + charScore x y, attachLast x y al)

            entry :: Int -> Int -> Entry
            entry 0 0 = (0, [("","")])
            entry i 0 = addChars (xs !! (i - 1)) '-' $ getEntry (i - 1) 0
            entry 0 j = addChars '-' (ys !! (j - 1)) $ getEntry 0 (j - 1)
            entry i j = (fst $ head result, concatMap snd result)
                where 
                    result = maximaBy fst [
                        addChars x y (getEntry (i-1) (j-1)),
                        addChars x '-' (getEntry (i-1) j),
                        addChars '-' y (getEntry i (j-1))]
                
                    x = xs !! (i - 1)
                    y = ys !! (j - 1)
            

    outputOptAlignments :: String -> String -> IO ()
    outputOptAlignments s1 s2 = do
        putStr "\n"
        putStrLn $ "There are " ++ (show . length) res ++ " optimal alignments!"
        recPrint res
        where
            res = optAlignments s1 s2
            recPrint :: [AlignmentType] -> IO ()
            recPrint [] = do putStr "\n"
            recPrint ((a,b):xs) = do
                putStr "\n"
                putStrLn $ spaceify a
                putStrLn $ spaceify b
                recPrint xs
                where
                    spaceify :: String -> String
                    spaceify [] = []
                    spaceify (c:cs) = c : ' ' : spaceify cs 
