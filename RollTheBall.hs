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
--instance Show Cell
 --   where show (Cell c) = putStrLn [c]


--dummyCell1 :: Cell
--dummyCell1 = Cell {cellValue = horPipe}
--dummyCell2 :: Cell
--dummyCell2 = Cell {cellValue = startRight}
--squares :: (A.Array Int Int)
--squares = A.array (1,10) [(x, x*x) | x <- [1..10]]
--arr123::(A.Array (Int, Int) Int)
--arr123 = A.array ((0, 0), (1, 1)) 
  --            [((0, 0), 1), ((0, 1), 2),
    --           ((1, 0), 3), ((1, 1), 4)]


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

--fillWithEndl::[Char]->[Char]
--fillWithEndl [] = [endl]
--fillWithEndl  x = intersperse endl x

--getCellList::Level -> [Char]
--getCellList level = map (\(Cell x)->x) (A.elems $ cells level)

-- foldr (\((a,b),(Cell c)) acc-> a:acc) [] (assocs $ emptySpaceArray (1,1))
--foldAux:: ((Int,Int),Cell)->[Char]-> [Char]
--foldAux ((a,b), (Cell c))
--              | a ==  
  
--linesToList :: [((Int,Int), Cell)] -> [Int]
--linesToList assoc = foldr (\((a,_),(Cell _)) acc-> a:acc) [] assoc

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

dummy:: [(Int,Int)]
dummy = [(0,0),(0,1),(-3,-3),(1,0),(1,1)]
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
                (repeat (Cell emptySpace )))
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

addCell :: (Char, Position) -> Level -> Level
addCell (newChar, pos) level = Level {rows = rows level
                                      ,columns = columns level
                                      ,cells = updateCellArr (newChar,pos) level} 

updateCellArr::(Char,Position) -> Level -> (A.Array (Int,Int) Cell)
updateCellArr (newChar, pos) level = (cells level) A.// [(pos, (Cell newChar))]


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
createLevel = undefined


{-
    *** TODO ***

    Mișcarea unei celule în una din cele 4 direcții 
    Schimbul se poate face doar dacă celula vecină e goală (emptySpace).
    Celulele de tip start și win sunt imutabile.

    Hint: Dacă nu se poate face mutarea puteți lăsa nivelul neschimbat.
-}

moveCell :: Position -> Directions -> Level -> Level
moveCell = undefined

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
