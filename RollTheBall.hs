{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE EmptyDataDecls, MultiParamTypeClasses,
             TypeSynonymInstances, FlexibleInstances,
             InstanceSigs #-}
module RollTheBall where
import Pipes
import ProblemState
import Data.Array as A

{-
    Direcțiile în care se poate mișca o piesa pe tablă
-}

data Directions = North | South | West | East
    deriving (Show, Eq, Ord)

{-
    Sinonim tip de date pentru reprezetarea unei perechi (Int, Int)
    care va reține coordonatele celulelor de pe tabla de joc
-}

type Position = (Int, Int)

{-
    Tip de date pentru reprezentarea celulelor tablei de joc
-}
data Cell = Cell Char deriving (Eq, Ord, Show)

showCell:: Cell -> IO()
showCell (Cell c) = putStrLn [c]


{-
    Tip de date pentru reprezentarea nivelului curent
-}
data Level = Level { rows::Int
                     ,columns::Int
                     ,cells::(A.Array (Int,Int) Cell) 
                   } deriving (Eq, Ord)

{-
    *** Optional *** 
  
    Dacă aveți nevoie de o funcționalitate particulară,
    instantiați explicit clasele Eq și Ord pentru Level.
    În cazul acesta, eliminați deriving (Eq, Ord) din Level.
-}

{-
    *** TODO ***

    Instanțiati Level pe Show. 
    Atenție! Fiecare linie este urmată de \n (endl in Pipes).
-}

--se pune un endl la final si la inceputul tabloului
instance Show Level 
    where show level = endl : (finishEndl $ formatMatrix level)

levelIndices :: Level -> [(Int,Int)]
levelIndices level = indices $ cells $ level

finishEndl :: [Char] -> [Char]
finishEndl xs = foldr(\x acc -> x:acc) [endl] xs

formatMatrix:: Level -> [Char]
formatMatrix level = replaceMarks (markNewline $ levelIndices level) (cells level)


markNewline :: [(Int,Int)] -> [(Int,Int)]
markNewline list = foldr (\x acc -> markNewlineAux x acc) [] list

markNewlineAux :: (Int,Int) -> [(Int,Int)] -> [(Int,Int)]
markNewlineAux x [] = x:[]
markNewlineAux (a,b) acc
        | a == (fst $ head acc) = (a,b):acc
        | otherwise = (a,b):minus3:acc

replaceMarks:: [(Int,Int)] -> (A.Array (Int,Int) Cell) -> [Char]
replaceMarks list ar = foldr (\x acc -> (replaceMarkAux x ar):acc) [] list

replaceMarkAux:: (Int,Int) -> (A.Array (Int,Int) Cell) -> Char
replaceMarkAux x ar
          | x == minus3 = endl
          | otherwise = cellValue $ (ar A.! x)

minus3 ::(Int,Int)
minus3 = (-3,-3) 

cellValue :: Cell -> Char
cellValue (Cell c) = c

{-
    *** TODO ***
    Primește coordonatele colțului din dreapta jos a hărții.
    Intoarce un obiect de tip Level în care tabla este populată
    cu EmptySpace. Implicit, colțul din stânga sus este (0,0)
-}

emptyLevel :: Position -> Level
emptyLevel dim = Level {rows = row + 1
                       ,columns = col + 1
                       ,cells = emptySpaceArray (row,col) }
         where row = (fst dim)
               col = (snd dim)


emptySpaceArray:: Position -> (A.Array (Int,Int) Cell)
emptySpaceArray pos = A.array ((0,0),(row, col))
           (zip (range ((0,0), (row,col))) 
                (repeat (Cell emptySpace)))
           where row = fst pos
                 col = snd pos



elemAt :: (A.Array (Int, Int) Cell) -> (Int, Int) -> Cell
elemAt arr pos = arr A.! pos

{-
    *** TODO ***

    Adaugă o celulă de tip Pipe în nivelul curent.
    Parametrul char descrie tipul de tile adăugat: 
        verPipe -> pipe vertical
        horPipe -> pipe orizontal
        topLeft, botLeft, topRight, botRight -> pipe de tip colt
        startUp, startDown, startLeft, startRight -> pipe de tip initial
        winUp, winDown, winLeft, winRight -> pipe de tip final
    Parametrul Position reprezintă poziția de pe hartă la care va fi adaugată
    celula, dacă aceasta este liberă (emptySpace).
-}

--functia adauga o noua celula daca pozitia este una valida
--si daca pe pozitia respectiva este un emptySpace
addCell :: (Char, Position) -> Level -> Level
addCell (newChar, pos) level 
                        | (fitsBounds pos level) == False = level
                        | otherwise = Level {rows = rows level
                                      ,columns = columns level
                                      ,cells = updateCellArr (newChar,pos) level} 

--functia primeste o pozitie si spune daca e o pozitie valida pe tabla
fitsBounds:: Position -> Level -> Bool
fitsBounds (row,col) level
                   | row < 0 = False
                   | col < 0 = False
                   | row > r = False
                   | col > c = False
                   | otherwise = True
                   where r = fst (snd (A.bounds (cells level)))
                         c = snd (snd (A.bounds (cells level)))


updateCellArr::(Char,Position) -> Level -> (A.Array (Int,Int) Cell)
updateCellArr (newChar, pos) level 
               | cellValueLevel pos level /= emptySpace = cells level
               | otherwise =  (cells level) A.// [(pos, (Cell newChar))]

cellValueLevel :: Position -> Level -> Char
cellValueLevel pos level = cellValue $ (cells level) A.! pos

{-
    *** TODO *** 

    Primește coordonatele colțului din dreapta jos al hărții și o listă de 
    perechi de tipul (caracter_celulă, poziția_celulei).
    Întoarce un obiect de tip Level cu toate celeule din listă agăugate pe
    hartă.
    Observatie: Lista primită ca parametru trebuie parcursă de la dreapta 
    la stanga.
-}
 

createLevel :: Position -> [(Char, Position)] -> Level
createLevel pos list= foldr foldrLevel (emptyLevel pos) list

foldrLevel :: (Char,Position) -> Level -> Level
foldrLevel (newChar, pos) level = addCell (newChar, pos) level
{-
    *** TODO ***

    Mișcarea unei celule în una din cele 4 direcții 
    Schimbul se poate face doar dacă celula vecină e goală (emptySpace).
    Celulele de tip start și win sunt imutabile.

    Hint: Dacă nu se poate face mutarea puteți lăsa nivelul neschimbat.
-}

moveCell :: Position -> Directions -> Level -> Level
moveCell oldPos dir level
                  | fitsBounds oldPos level == False = level
                  | fitsBounds nextPos level == False = level
                  | cellValue (elemAt (cells level) oldPos) == startRight = level
                  | cellValue (elemAt (cells level) oldPos) == startLeft = level
                  | cellValue (elemAt (cells level) oldPos) == startDown = level
                  | cellValue (elemAt (cells level) oldPos) == startUp = level
                  | cellValue (elemAt (cells level) oldPos) == winUp = level
                  | cellValue (elemAt (cells level) oldPos) == winDown = level
                  | cellValue (elemAt (cells level) oldPos) == winRight = level
                  | cellValue (elemAt (cells level) oldPos) == winLeft = level
                  | cellValue (elemAt (cells level) nextPos) /= emptySpace = level
                  | otherwise = changeCell (oldPos, (Cell emptySpace)) (changeCell (nextPos, cell) level)
                  where nextPos = nextPosition oldPos dir
                        cell = (cells level) A.! oldPos


nextPosition:: Position -> Directions -> Position
nextPosition (a,b) dir
               | dir == West = (a,b-1)
               | dir == East = (a,b+1)
               | dir == North = (a-1,b)
               | dir == South = (a+1,b)
               | otherwise = (a,b)


changeCell::(Position, Cell) -> Level -> Level
changeCell (pos, cell) level = Level {
                                         rows = r,
                                         columns = c,
                                         cells = (cells level) A.// [(pos,cell)]
                                       }
                           where r = rows level
                                 c = columns level
{-
    *** HELPER ***

    Verifică dacă două celule se pot conecta.
    Atenție: Direcția indică ce vecin este a
    doua celulă pentru prima.

    ex: connection botLeft horPipe East = True (╚═)
        connection horPipe botLeft East = False (═╚)
-}
connection :: Cell -> Cell -> Directions -> Bool
connection (Cell x) (Cell y) dir
        | x == horPipe = checkHorPipe (y,dir)
        | otherwise = False
      


horPipeList :: [(Char, Directions)]
horPipeList = [(topLeft, West), (botLeft, West), (botRight, East), (topRight, East),
               (startLeft, East), (startRight, West), (winLeft,East), (winRight,West)]



checkHorPipe :: (Char,Directions) -> Bool
checkHorPipe x = x `elem` horPipeList

{-
    *** TODO ***

    Va returna True dacă jocul este câștigat, False dacă nu.
    Va verifica dacă celulele cu Pipe formează o cale continuă de la celula
    de tip inițial la cea de tip final.
    Este folosită în cadrul Interactive.
-}
wonLevel :: Level -> Bool
wonLevel = undefined

instance ProblemState Level (Position, Directions) where
    successors = undefined
    isGoal = undefined
    reverseAction = undefined
