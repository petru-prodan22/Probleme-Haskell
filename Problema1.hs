import System.IO (hFlush, stdout)

subCuvinte :: String -> [String]
subCuvinte "" = [""]
subCuvinte (x:xs) = [x:sub | sub <- rest] ++ rest
  where rest = subCuvinte xs

estesubCuvant1 :: String -> String -> Bool
estesubCuvant1 xs ys = xs `elem` subCuvinte ys

estesubCuvant2 :: String -> String -> Bool
estesubCuvant2 "" _ = True
estesubCuvant2 _ "" = False
estesubCuvant2 (x:xs) (y:ys)
  | x == y    = estesubCuvant2 xs ys
  | otherwise = estesubCuvant2 (x:xs) ys

main :: IO ()
main = do
    putStrLn "--- Verificare Subcuvant ---"
    putStr "Introdu subcuvantul cautat: "
    hFlush stdout
    s1 <- getLine
    putStr "Introdu sirul sursa: "
    hFlush stdout
    s2 <- getLine
    
    let rezultat = estesubCuvant2 s1 s2
    putStrLn $ "Rezultat: " ++ show rezultat