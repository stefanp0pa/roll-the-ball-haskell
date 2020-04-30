{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE EmptyDataDecls, MultiParamTypeClasses,
             TypeSynonymInstances, FlexibleInstances,
             InstanceSigs #-}

module Pipes where
{--
	Acest modul furnizeaza caracterele speciale
    pentru afisarea celulelor nivelului.
--}

-- XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX

horPipe :: Char
horPipe = '═'

verPipe :: Char
verPipe = '║'

-- XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX

topLeft :: Char
topLeft = '╔'

botLeft :: Char
botLeft = '╚'

-- XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX

botRight :: Char
botRight = '╝'

topRight :: Char
topRight = '╗'

-- XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX

emptySpace :: Char
emptySpace =  '░'

emptyCell :: Char
emptyCell = '▓'

-- XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX

startUp :: Char
startUp = '┴'

startDown :: Char
startDown = '┬'

startLeft :: Char
startLeft = '┤'

startRight :: Char
startRight = '├'


-- XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX

winUp :: Char
winUp = '╨'

winDown :: Char--
winDown = '╥'

winLeft :: Char
winLeft = '╡'

winRight :: Char
winRight = '╞'

-- XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX

endl :: Char
endl = '\n'

startCells :: [Char]
startCells = [startUp, startDown, startLeft, startRight]

winningCells :: [Char]
winningCells = [winUp, winDown, winLeft, winRight]
