{-# LANGUAGE FunctionalDependencies, GADTs#-}

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

data TermI = LitI Int | Succ TermI deriving Show
data TermB = LitB Bool | IsZero TermI deriving Show
data Term = If TermB Term Term | TB TermB | TI TermI deriving Show
data Res = RI Int | RB Bool

-- Functional Dependency
class Eval a b | a -> b where
    eval :: a -> b

instance Eval TermI Int where
    eval :: TermI -> Int
    eval (LitI i) = i
    eval (Succ t) = 1 + eval t

instance Eval TermB Bool where
    eval :: TermB -> Bool
    eval (LitB b) = b
    eval (IsZero t) = 0 == eval t

instance Eval Term Res where
    eval :: Term -> Res
    eval (If b t1 t2) = if eval b then eval t1 else eval t2
    eval (TB t) = RB (eval t)
    eval (TI t) = RI (eval t)

-- GADT Functional Dependent
data Term' a where
    Lit' :: Int -> Term' Int
    LitB' :: Bool -> Term' Bool
    Succ' :: Term' Int -> Term' Int
    IsZero' :: Term' Int -> Term' Bool
    If' :: Term' Bool -> Term' a -> Term' a -> Term' a

class Eval' a b | a -> b where
  eval' :: a -> b

instance Eval' (Term' a) a where
  eval' :: Term' a -> a
  eval' (Lit' i) = i
  eval' (LitB' b) = b
  eval' (Succ' t) = 1 + eval' t
  eval' (IsZero' t) = 0 == eval' t
  eval' (If' b t1 t2) = if eval' b then eval' t1 else eval' t2

gadtFunctionalDependent :: IO ()
gadtFunctionalDependent = do
    let term1 = Lit' 5
        term2 = Succ' (Lit' 3)
        term3 = IsZero' (Lit' 0)
        term4 = If' (LitB' True) (Lit' 10) (Lit' 20)
        term5 = If' (IsZero' (Lit' 3)) (Lit' 30) (Lit' 40)

    putStrLn "Expression 1:"
    print $ eval' term1

    putStrLn "Expression 2:"
    print $ eval' term2

    putStrLn "Expression 3:"
    print $ eval' term3

    putStrLn "Expression 4:"
    print $ eval' term4

    putStrLn "Expression 5:"
    print $ eval' term5
