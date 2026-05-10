import System.IO
import Data.Char (isAlphaNum)

inlocuiesteSeparator :: Char -> Char
inlocuiesteSeparator c = 
    if isAlphaNum c 
    then c 
    else ' '

obtineCuvinte :: String -> [String]
obtineCuvinte text = words [inlocuiesteSeparator c | c <- text]

numaraAparitii :: Eq a => a -> [a] -> Int
numaraAparitii valoare lista = length [x | x <- lista, x == valoare]

elementeUnice :: Eq a => [a] -> [a]
elementeUnice [] = []
elementeUnice (x:xs) = x : elementeUnice [y | y <- xs, y /= x]

afiseazaStatistici :: [String] -> [String] -> IO ()
afiseazaStatistici [] _ = return ()
afiseazaStatistici (cuvantUnic : restulUnice) toateCuvintele = do
    let numar = numaraAparitii cuvantUnic toateCuvintele
    putStrLn (cuvantUnic ++ " \t-> " ++ show numar ++ " aparitii")
    afiseazaStatistici restulUnice toateCuvintele

main :: IO ()
main = do
    textDinFisier <- readFile "text.txt"
    
    let toateCuvintele = obtineCuvinte textDinFisier
    
    let cuvinteFaraDuplicate = elementeUnice toateCuvintele
    
    putStrLn "--- Frecventa cuvintelor din fisier ---"
    afiseazaStatistici cuvinteFaraDuplicate toateCuvintele