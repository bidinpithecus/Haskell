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

data BST a = BSTNode (BST a) a (BST a) | BSTLeaf

instance Show a => Show (BST a) where
  show tree = visualize tree 0
    where
      visualize BSTLeaf _ = ""
      visualize (BSTNode left val right) depth =
        visualize right (depth + 1) ++
        replicate (depth * 4) ' ' ++ show val ++ "\n" ++
        visualize left (depth + 1)

insertBST :: Ord a => BST a -> a -> BST a
insertBST BSTLeaf a = BSTNode BSTLeaf a BSTLeaf
insertBST (BSTNode l x r) a 
  | a < x = BSTNode (insertBST l a) x r
  | a > x = BSTNode l x (insertBST r a)
  | otherwise = BSTNode l x r

preorderBST :: BST a -> [a]
preorderBST BSTLeaf = []
preorderBST (BSTNode l x r) = [x] ++ preorderBST l ++ preorderBST r

inorderBST :: BST a -> [a]
inorderBST BSTLeaf = []
inorderBST (BSTNode l x r) = inorderBST l ++ [x] ++ inorderBST r

postorderBST :: BST a -> [a]
postorderBST BSTLeaf = []
postorderBST (BSTNode l x r) = postorderBST l ++ postorderBST r ++ [x]

minBST :: BST a -> Maybe a
minBST (BSTNode BSTLeaf a _) = Just a
minBST (BSTNode l _ _) = minBST l
minBST _ = Nothing

deleteBST :: Ord a => BST a -> a -> BST a
deleteBST BSTLeaf _ = BSTLeaf
deleteBST node@(BSTNode BSTLeaf x BSTLeaf) a
  | a == x = BSTLeaf
  | otherwise = node
deleteBST (BSTNode l x r) a
  | a < x = BSTNode l x (deleteBST r a)
  | a > x = BSTNode (deleteBST l a) x r
  | otherwise = case r of
    BSTLeaf -> l
    _ -> BSTNode l succ (deleteBST r succ) where Just succ = minBST r


-- Red Black Tree

data RBColor = Red | Black deriving Show
data RBT a = RBNode RBColor (RBT a) a (RBT a) | RBLeaf

instance Show a => Show (RBT a) where
  show :: Show a => RBT a -> String
  show tree = visualize tree 0
    where
      visualize RBLeaf _ = ""
      visualize (RBNode color left val right) depth =
        visualize right (depth + 1) ++
        replicate (depth * 4) ' ' ++ show val ++ " (" ++ show color ++ ")" ++ "\n" ++
        visualize left (depth + 1)

buildRed :: RBT a -> a -> RBT a -> a -> RBT a -> a -> RBT a -> RBT a
buildRed a x b y c z d = RBNode Red (RBNode Black a x b) y (RBNode Black c z d)

balance :: RBT a -> RBT a
-- Case 1
balance (RBNode Black (RBNode Red (RBNode Red a x b) y c) z d) = buildRed a x b y c z d
-- Case 2
balance (RBNode Black (RBNode Red a x (RBNode Red b y c)) z d) = buildRed a x b y c z d
-- Case 3
balance (RBNode Black a x (RBNode Red b y (RBNode Red c z d))) = buildRed a x b y c z d
-- Case 4
balance (RBNode Black a x (RBNode Red (RBNode Red b y c) z d)) = buildRed a x b y c z d
balance any = any

insertRBHelper :: Ord a => RBT a -> a -> RBT a
insertRBHelper RBLeaf a = RBNode Red RBLeaf a RBLeaf
insertRBHelper (RBNode color l x r) a
  | a < x = balance (RBNode color (insertRBHelper l a) x r)
  | a > x = balance (RBNode color l x (insertRBHelper r a))
  | otherwise = RBNode color l x r

makeBlack :: RBT a -> RBT a
makeBlack RBLeaf = RBLeaf
makeBlack (RBNode _ l x r) = RBNode Black l x r

insertRB :: Ord a => RBT a -> a -> RBT a
insertRB tree a = makeBlack (insertRBHelper tree a)

-- Mathematical expressions

data Expr = Expr :+: Expr | Expr :-: Expr | Expr :*: Expr | Expr :/: Expr | Const Int deriving (Show)

evalExpr :: Expr -> Int
evalExpr (Const i) = i
evalExpr (e1 :*: e2) = evalExpr e1 * evalExpr e2
evalExpr (e1 :/: e2) = evalExpr e1 `div` evalExpr e2
evalExpr (e1 :+: e2) = evalExpr e1 + evalExpr e2
evalExpr (e1 :-: e2) = evalExpr e1 - evalExpr e2
