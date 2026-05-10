import System.IO (hFlush, stdout)
import Data.List (group, intercalate)

prim :: Int -> Bool
prim p
  | p <= 1    = False
  | otherwise = null [x | x <- [2 .. p `div` 2], p `mod` x == 0]

divPrimi :: Int -> [Int]
divPrimi n = extrageFactori n 2
  where
    extrageFactori 1 _ = []
    extrageFactori numar divizor
      | numar `mod` divizor == 0 = divizor : extrageFactori (numar `div` divizor) divizor
      | otherwise                = extrageFactori numar (divizor + 1)

afisDescomp :: [Int] -> String
afisDescomp factori = intercalate "*" (map formateazaGrup grupuriDeFactori)
  where
    grupuriDeFactori = group factori
    
    formateazaGrup grup
      | exponent == 1 = show baza
      | otherwise     = show baza ++ "^" ++ show exponent
      where
        baza = head grup
        exponent = length grup

main :: IO ()
main = do
    putStrLn "--- Descompunere in Factori Primi ---"
    putStr "Introdu un numar natural n: "
    hFlush stdout
    input <- getLine
    let n = read input :: Int
    
    if n <= 1
        then putStrLn "Numarul trebuie sa fie strict mai mare ca 1."
        else do
            let listaFactori = divPrimi n
            putStrLn $ "Lista divizorilor (punctul b): " ++ show listaFactori
            putStrLn $ "Descompunerea finala (punctul c): " ++ afisDescomp listaFactori