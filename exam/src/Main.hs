module Main where

import BaseN
import Rotate
import Tree

main :: IO ()
main = do
  let tree = Node (Node Leaf 1 (Node Leaf 2 Leaf)) 3 (Node Leaf 4 Leaf)
  print $ inOrder tree
  print $ preOrder tree
  print $ postOrder tree

