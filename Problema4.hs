import System.IO
import Data.List (sort)

type Nume = String
type Nota = Int
type Catalog = [(Nume, Nota)]

afiseazaStudent :: (Nume, Nota) -> IO ()
afiseazaStudent (nume, nota) = putStrLn (nume ++ "\t\t\t" ++ show nota)

afiseazaCatalog :: Catalog -> IO ()
afiseazaCatalog [] = return ()
afiseazaCatalog (student:restul) = do
    afiseazaStudent student
    afiseazaCatalog restul

invers :: (Nume, Nota) -> (Nota, Nume)
invers (nume, nota) = (nota, nume)

normal :: (Nota, Nume) -> (Nume, Nota)
normal (nota, nume) = (nume, nota)

sortareNote :: Catalog -> Catalog
sortareNote catalog = map normal (reverse (sort (map invers catalog)))

main :: IO ()
main = do
    text <- readFile "catalog.txt"
    let catalog = read text :: Catalog
    
    putStrLn "--- 1. Catalog Alfabetic ---"
    afiseazaCatalog (sort catalog)
    
    putStrLn "\n--- 2. Catalog Descrescator (Note) ---"
    afiseazaCatalog (sortareNote catalog)
    
    putStrLn "\n--- 3. Statistici Note ---"
    let note_1_4  = length [nota | (nume, nota) <- catalog, nota >= 1, nota <= 4]
    let note_5_8  = length [nota | (nume, nota) <- catalog, nota >= 5, nota <= 8]
    let note_9_10 = length [nota | (nume, nota) <- catalog, nota >= 9, nota <= 10]
    
    putStrLn ("Intre 1 si 4:  " ++ show note_1_4)
    putStrLn ("Intre 5 si 8:  " ++ show note_5_8)
    putStrLn ("Intre 9 si 10: " ++ show note_9_10)
    
    putStrLn "\n--- 4. Verificare Erori ---"
    let noteGresite = length [nota | (nume, nota) <- catalog, nota < 1 || nota > 10]
    if noteGresite > 0
    then putStrLn "EROARE: Exista note in afara intervalului 1-10!"
    else putStrLn "Toate notele sunt in regula."