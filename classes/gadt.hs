-- Generalized Algebraic Data Types

{-# LANGUAGE GADTs #-}

data Term a where
    Lit :: Int -> Term Int
    LitB :: Bool -> Term Bool
    Succ :: Term Int -> Term Int
    IsZero :: Term Int -> Term Bool
    If :: Term Bool -> Term a -> Term a -> Term a

eval :: Term a -> a
eval (Lit i) = i
eval (LitB b) = b
eval (Succ t) = 1 + eval t
eval (IsZero t) = 0 == eval t
eval (If b t1 t2) = if eval b then eval t1 else eval t2

{-
Ensuring part of Red-Black Tree correctness in its type definition:
    - Leaf nodes must be black.
    - Red nodes must have only black children.
    - Black nodes do not have restrictions on the color of their children.
-}

data Red
data Black

data RBTree a c where
    Leaf :: RBTree a Black
    Red :: RBTree a Black -> a -> RBTree a Black -> RBTree a Red
    Black :: RBTree a c -> a -> RBTree a c -> RBTree a Black
