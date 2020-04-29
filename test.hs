data A = MakeA Int Float deriving Show
newtype B = MakeB A deriving Show

data Pereche = P { firstValue :: Int, secondValue :: Int }

getFirst pair@(P firstValue secondValue) = firstValue pair