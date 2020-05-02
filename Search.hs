{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Search where

import ProblemState
{-
    *** TODO ***

    Tipul unei nod utilizat în procesul de căutare. Recomandăm reținerea unor
    informații legate de:

    * stare;
    * acțiunea care a condus la această stare;
    * nodul părinte, prin explorarea căruia a fost obținut nodul curent;
    * adâncime
    * copiii, ce vor desemna stările învecinate
-}

data Node s a = Node { state:: s
                      ,action:: Maybe a
                      ,parent:: Maybe (Node s a)
                      ,depth:: Int
                      ,children:: [Node s a] 
                     } deriving (Show)

{-
    *** TODO ***
    Gettere folosite pentru accesul la câmpurile nodului
-}
nodeState :: Node s a -> s
nodeState n = state n

nodeParent :: Node s a -> Maybe (Node s a)
nodeParent n = parent n

nodeDepth :: Node s a -> Int
nodeDepth n = depth n

nodeAction :: Node s a -> Maybe a
nodeAction n = action n

nodeChildren :: Node s a -> [Node s a]
nodeChildren n = children n



{-
    *** TODO ***

    Generarea întregului spațiu al stărilor
    Primește starea inițială și creează nodul corespunzător acestei stări,
    având drept copii nodurile succesorilor stării curente.
-}

--fluxTest:: Int -> [Int]
--fluxTest i = i : [(fluxTest i),(fluxTest i)]

createStateSpace :: (ProblemState s a, Eq s) => s -> Node s a
createStateSpace initState = root
                    where root =  Node {state = initState --s
                                       ,action = Nothing -- Maybe a
                                       ,parent = Nothing -- Maybe (Node s a)
                                       ,depth = 0        -- Int
                                       ,children = childrenNodes root}





childrenNodes :: (ProblemState s a, Eq s) => Node s a -> [(Node s a)]
childrenNodes parentNode = [child | (act,st) <- (successors (state parentNode)),
              let child = Node {state = st ,action = Just act, parent = Just parentNode ,depth = (depth parentNode) + 1 ,children = childrenNodes child}]

--    successors :: s -> [(a, s)]



{-
    *** TODO ***
   
    Primește un nod inițial și întoarce un flux de perechi formate din:
    * lista nodurilor adăugate în frontieră la pasul curent
    * frontiera

-}

bfs :: Ord s => Node s a -> [([Node s a], [Node s a])]
bfs node = bfsAux [node] []

-- queue -- visited 
bfsAux:: Ord s => [Node s a] -> [s] -> [([Node s a], [Node s a])]
bfsAux (x:xs) visited 
          | (state x) `elem` visited = []
          | otherwise = (currChildren, concat [xs,currChildren]) : (bfsAux (concat [xs,currChildren]) ((state x):visited))
          where currChildren = filter (\n -> (((state n) `elem` visited) == False))  (children x)

{-
    *** TODO ***
  
    Primește starea inițială și finală și întoarce o pereche de noduri, reprezentând
    intersecția dintre cele două frontiere.
-}

bidirBFS :: Ord s => Node s a -> Node s a -> (Node s a, Node s a)
bidirBFS = undefined


{-
    *** TODO ***

    Pornind de la un nod, reface calea către nodul inițial, urmând legăturile
    către părinți.

    Întoarce o listă de perechi (acțiune, stare), care pornește de la starea inițială
    și se încheie în starea finală.

-}

extractPath :: Node s a -> [(Maybe a, s)]
extractPath = undefined



{-
    *** TODO ***

    Pornind de la o stare inițială și una finală, se folosește de bidirBFS pentru a găsi
    intersecția dintre cele două frontiere și de extractPath pentru a genera calea.

    Atenție: Pentru calea gasită în a doua parcurgere, trebuie să aveți grijă la a asocia
    corect fiecare stare cu acțiunea care a generat-o.

    Întoarce o listă de perechi (acțiune, stare), care pornește de la starea inițială
    și se încheie în starea finală.
-}

solve :: (ProblemState s a, Ord s)
      => s          -- Starea inițială de la care se pornește
      -> s          -- Starea finală la care se ajunge
      -> [(Maybe a, s)]   -- Lista perechilor
solve = undefined







--createChildren :: (ProblemState s a, Eq s) => (Maybe a,s) -> Int -> [Node s a]
--createChildren (parentAct,parentSt) parDepth = foldr (\(succAct, succSt) -> (createSpaceAux (Just succAct, succSt) (parentAct, parentSt, parDepth))) [] succPairs
--            where succPairs = successors parentSt


--createSpaceAux:: (ProblemState s a, Eq s)=> (Maybe a,s) -> (Maybe a,s,Int) -> Node s a
--createSpaceAux (act,st) ((Just parAct), parSt, parDepth) = Node {state = st
 --                                                        ,action = act
 --                                                        ,parent = defParent
 --                                                        ,depth = parDepth + 1
 --                                                        ,children = (createChildren (act, st) parDepth)}
 --                           where defParent = Just (Node parAct parSt)
