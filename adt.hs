-- Basic

data WeekDay = Mon | Tue | Wed | Thu | Fri | Sat | Sun deriving (Show)

nextDay :: WeekDay -> WeekDay
nextDay Mon = Tue
nextDay Tue = Wed
nextDay Wed = Thu
nextDay Thu = Fri
nextDay Fri = Sat
nextDay Sat = Sun
nextDay Sun = Mon

nextWeekDay :: WeekDay -> WeekDay
nextWeekDay Fri = Mon
nextWeekDay Sat = Mon
nextWeekDay day = nextDay day

nextWeekend :: WeekDay -> WeekDay
nextWeekend Sat = Sun
nextWeekend _ = Sat


-- Binary Search Tree

data Bst a = Node a (Bst a) (Bst a) | Leaf deriving (Show)

insertChild :: Ord a => Bst a -> a -> Bst a
insertChild Leaf a = Node a Leaf Leaf
insertChild (Node x l r) a 
  | a < x = Node x (insertChild l a) r
  | a > x = Node x l (insertChild r a)
  | otherwise = Node x l r

preorder :: Bst a -> [a]
preorder Leaf = []
preorder (Node x l r) = [x] ++ preorder l ++ preorder r

inorder :: Bst a -> [a]
inorder Leaf = []
inorder (Node x l r) = inorder l ++ [x] ++ inorder r

postorder :: Bst a -> [a]
postorder Leaf = []
postorder (Node x l r) = postorder l ++  postorder r ++ [x]

minInBst :: Bst a -> Maybe a
minInBst (Node a Leaf _) = Just a
minInBst (Node _ l _) = minInBst l
minInBst _ = Nothing


-- Mathematical expressions

data Expr = Expr :+: Expr | Expr :-: Expr | Expr :*: Expr | Expr :/: Expr | Const Int deriving (Show)

evalExpr :: Expr -> Int
evalExpr (Const i) = i
evalExpr (e1 :*: e2) = evalExpr e1 * evalExpr e2
evalExpr (e1 :/: e2) = evalExpr e1 `div` evalExpr e2
evalExpr (e1 :+: e2) = evalExpr e1 + evalExpr e2
evalExpr (e1 :-: e2) = evalExpr e1 - evalExpr e2
