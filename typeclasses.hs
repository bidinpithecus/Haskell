data Tree a = Node (Tree a) a (Tree a) | Leaf
data WeekDay = Sun | Mon | Tue | Wed | Thu | Fri | Sat

class Len a where 
  len :: a -> Int

instance Len (Maybe a) where
  len :: Maybe a -> Int
  len Nothing = 0
  len (Just _) = 1

instance Len [a] where
  len :: [a] -> Int
  len = length

instance Len (Tree a) where
  len :: Tree a -> Int
  len Leaf = 0
  len (Node l _ r) = 1 + len l + len r

instance (Show a) => Show (Tree a) where
  show :: Show a => Tree a -> String
  show Leaf = "-"
  show (Node l x r) = "(" ++ show l ++ ") " ++ show x ++ " (" ++ show r ++ ")"

instance Show WeekDay where
  show :: WeekDay -> String
  show Sun = "Domingo"
  show Mon = "Segunda"
  show Tue = "Terça"
  show Wed = "Quarta"
  show Thu = "Quinta"
  show Fri = "Sexta"
  show Sat = "Sábado"
