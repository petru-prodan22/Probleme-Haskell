import System.IO (hFlush, stdout)

cmmdc :: Int -> Int -> Int
cmmdc a 0 = a
cmmdc a b = cmmdc b (a `mod` b)

cmmmc :: Int -> Int -> Int
cmmmc 0 0 = 0
cmmmc a b = (a * b) `div` cmmdc a b

cmmdcNr :: [Int] -> Int
cmmdcNr [] = 0                          
cmmdcNr [x] = x                         
cmmdcNr (x:xs) = cmmdc x (cmmdcNr xs)   

cmmmcNr :: [Int] -> Int
cmmmcNr [] = 0                          
cmmmcNr [x] = x                         
cmmmcNr (x:xs) = cmmmc x (cmmmcNr xs)   

main :: IO ()
main = do
    putStrLn "--- Calcul CMMDC si CMMMC ---"
    putStr "Introdu numerele separate prin spatiu (ex: 12 18 24): "
    hFlush stdout
    
    input <- getLine
    
    let numere = map read (words input) :: [Int]
    
    if null numere
    then putStrLn "Nu ai introdus niciun numar!"
    else do
        putStrLn $ "\nLista numerelor introduse: " ++ show numere
        putStrLn $ "CMMDC este: " ++ show (cmmdcNr numere)
        putStrLn $ "CMMMC este: " ++ show (cmmmcNr numere)