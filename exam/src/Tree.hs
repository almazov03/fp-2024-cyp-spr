module Tree where

data Tree a = Leaf | Node (Tree a) a (Tree a) deriving (Show, Eq)

instance Foldable Tree where
  foldMap _ Leaf = mempty
  foldMap f (Node left x right) = foldMap f left `mappend` f x `mappend` foldMap f right

  foldr _ z Leaf = z
  foldr f z (Node left x right) = foldr f (f x (foldr f z right)) left

inOrder :: Tree a -> [a]
inOrder Leaf = []
inOrder (Node left x right) = inOrder left ++ [x] ++ inOrder right

preOrder :: Tree a -> [a]
preOrder Leaf = []
preOrder (Node left x right) = [x] ++ preOrder left ++ preOrder right

postOrder :: Tree a -> [a]
postOrder Leaf = []
postOrder (Node left x right) = postOrder left ++ postOrder right ++ [x]

