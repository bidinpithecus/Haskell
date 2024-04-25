{-# LANGUAGE GADTs #-}

evenDoCrisCris x = rem x 2 == 0

-- Function composition (.) operator
oddDoCrisCris = not . evenDoCrisCris

mapDoCrisCris f [] = []
mapDoCrisCris f (x : xs) = f x : mapDoCrisCris f xs

data Tree a where
  Node :: Tree a -> a -> Tree a -> Tree a
  Leaf :: Tree a

instance Functor Tree where
  fmap :: (a -> b) -> Tree a -> Tree b
  fmap f Leaf = Leaf
  fmap f (Node l a r) = Node (fmap f l) (f a) (fmap f r)
