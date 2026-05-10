import Data.List (sort)
import Data.Char (toLower)


rotatii :: [a] -> [[a]]
rotatii [] = []
rotatii xs = [drop i xs ++ take i xs | i <- [0 .. length xs - 1]]


cuvinteDeLegatura :: [String]
cuvinteDeLegatura = ["in", "din", "sau", "si", "o", "un"]


esteLegatura :: String -> Bool
esteLegatura w = map toLower w `elem` cuvinteDeLegatura

rotatiiTitlu :: String -> [String]
rotatiiTitlu titlu = 
  let listaCuvinte = words titlu          
      toateRotatiile = rotatii listaCuvinte 
  in [unwords rotatie | rotatie <- toateRotatiile, not (esteLegatura (head rotatie))]


indexTitluri :: [String] -> [String]
indexTitluri bazaDeDate = sort (concatMap rotatiiTitlu bazaDeDate)

cautareTitlu :: [String] -> String -> [String]
cautareTitlu bazaDeDate cheie = filter (\titlu -> cheie `elem` words titlu) bazaDeDate
