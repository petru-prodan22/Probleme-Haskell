import System.IO (hFlush, stdout)
import Data.Char (isAlpha, isAlphaNum)

pal :: (a -> Bool) -> [a] -> ([a], [a])
pal _ [] = ([], [])
pal p (x:xs) =
  if p x
  then
      let (ys, zs) = pal p xs
      in (x:ys, zs)  
  else
      ([], x:xs)    

esteCuvant :: String -> Bool
esteCuvant "" = False              
esteCuvant [x] = isAlphaNum x      
esteCuvant (x:xs) =
  if isAlphaNum x
  then esteCuvant xs               
  else False                       

esteLitera :: Char -> Bool
esteLitera = isAlpha

nuEsteLitera :: Char -> Bool
nuEsteLitera c = not (isAlpha c)

cuvinte :: String -> [String]
cuvinte "" = []
cuvinte sir =
  let (separatori, rest1) = pal nuEsteLitera sir
  in
      if rest1 == ""
      then [] 
      else
          let (cuvant, rest2) = pal esteLitera rest1
          in cuvant : cuvinte rest2 

main :: IO ()
main = do
    putStrLn "Introdu un sir de caractere de la tastatura:"
    hFlush stdout
    input <- getLine

    let listaFinala = cuvinte input
    putStrLn "Cuvintele gasite sunt:"
    print listaFinala