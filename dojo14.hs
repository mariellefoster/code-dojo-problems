makeNum :: Int -> String
makeNum n = replicate n '5'


nextNum :: String -> String
nextNum [n] = ['3']
nextNum (n:ns) = if head ns == '3' then '3':ns else n:(nextNum ns)


makeCandidates :: Int -> [String]
makeCandidates n = take (n+1) $ iterate nextNum (makeNum n)


sumNumChar :: Char -> String -> Int
sumNumChar a num = length $ filter (==a) num

-- gets passed in a number to check the subcases
sherLogic :: Int -> [String]
sherLogic n = dropWhile f (makeCandidates n)
    where f = (\ x -> not((sumNumChar '3' x `mod` 5 == 0) && (sumNumChar '5' x `mod` 3 == 0)))
          --cands = [x++y | b <- [0..n], a <- [0..n],(a+b ==n),  let x = (replicate a '5'), let y = (replicate b '3')]
 


readNLines :: Int -> IO ()
readNLines 0 = return ()
readNLines cases = do
    n <- getLine 
    let c = read n :: Int
    case sherLogic c of (x:xs) -> do
                            putStrLn x
                            readNLines (cases-1)
                        [] -> do
                            putStrLn "-1"
                            readNLines (cases-1)



main :: IO ()
main = do
    t <- getLine
    let cases = read t :: Int
    readNLines cases


